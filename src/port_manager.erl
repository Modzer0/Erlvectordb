%% Copyright (C) 2026  ErlVectorDB Contributors
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.

-module(port_manager).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    allocate_ports/1,
    release_ports/1,
    get_port_status/0,
    get_service_port/1,
    force_restart_service/1,
    reload_config/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Types
-type service_name() :: mcp_server | oauth_server | rest_api_server.
-type port_config() :: #{
    service => service_name(),
    preferred_port => integer(),
    port_range => {integer(), integer()},
    required => boolean()
}.
-type port_allocation() :: #{
    service => service_name(),
    allocated_port => integer(),
    status => bound | failed | released
}.

%% Records
-record(port_allocation, {
    service :: service_name(),
    port :: integer(),
    pid :: pid() | undefined,
    status :: allocated | bound | failed | released,
    allocated_at :: integer(),
    bind_attempts :: integer(),
    last_error :: term() | undefined
}).

-record(state, {
    allocations :: #{service_name() => #port_allocation{}},
    registry_pid :: pid() | undefined,
    config :: #{service_name() => port_config()}
}).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

allocate_ports(Services) when is_list(Services) ->
    gen_server:call(?SERVER, {allocate_ports, Services}).

release_ports(Services) when is_list(Services) ->
    gen_server:call(?SERVER, {release_ports, Services}).

get_port_status() ->
    gen_server:call(?SERVER, get_port_status).

get_service_port(Service) when is_atom(Service) ->
    gen_server:call(?SERVER, {get_service_port, Service}).

force_restart_service(Service) when is_atom(Service) ->
    gen_server:call(?SERVER, {force_restart_service, Service}).

reload_config() ->
    gen_server:call(?SERVER, reload_config).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    process_flag(trap_exit, true),
    
    % Start port registry
    {ok, RegistryPid} = port_registry:start_link(),
    
    % Load configuration
    Config = port_config:load_config(),
    
    State = #state{
        allocations = #{},
        registry_pid = RegistryPid,
        config = Config
    },
    
    {ok, State}.

handle_call({allocate_ports, Services}, _From, State) ->
    {Reply, NewState} = do_allocate_ports(Services, State),
    {reply, Reply, NewState};

handle_call({release_ports, Services}, _From, State) ->
    {Reply, NewState} = do_release_ports(Services, State),
    {reply, Reply, NewState};

handle_call(get_port_status, _From, State) ->
    Status = get_status_info(State),
    {reply, {ok, Status}, State};

handle_call({get_service_port, Service}, _From, State) ->
    Reply = case maps:get(Service, State#state.allocations, undefined) of
        undefined ->
            {error, service_not_allocated};
        #port_allocation{port = Port, status = bound} ->
            {ok, Port};
        #port_allocation{status = Status} ->
            {error, {service_not_bound, Status}}
    end,
    {reply, Reply, State};

handle_call({force_restart_service, Service}, _From, State) ->
    {Reply, NewState} = do_force_restart_service(Service, State),
    {reply, Reply, NewState};

handle_call(reload_config, _From, State) ->
    % Reload configuration
    NewConfig = port_config:load_config(),
    NewState = State#state{config = NewConfig},
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, Reason}, State) ->
    case Pid of
        RegistryPid when RegistryPid =:= State#state.registry_pid ->
            error_logger:error_msg("Port registry died: ~p~n", [Reason]),
            {stop, {port_registry_died, Reason}, State};
        _ ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    % Clean up any bound ports
    Services = maps:keys(State#state.allocations),
    {_Reply, _NewState} = do_release_ports(Services, State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

do_allocate_ports(Services, State) ->
    Config = State#state.config,
    Results = lists:map(fun(Service) ->
        ServiceConfig = maps:get(Service, Config, get_default_config(Service)),
        allocate_single_port(Service, ServiceConfig, State)
    end, Services),
    
    % Check if all allocations succeeded
    case lists:all(fun({Status, _}) -> Status =:= ok end, Results) of
        true ->
            % All succeeded, update state
            NewAllocations = lists:foldl(fun({ok, Allocation}, Acc) ->
                Service = Allocation#port_allocation.service,
                maps:put(Service, Allocation, Acc)
            end, State#state.allocations, Results),
            
            NewState = State#state{allocations = NewAllocations},
            {{ok, extract_port_info(Results)}, NewState};
        false ->
            % Some failed, clean up any successful allocations
            SuccessfulServices = [Service || {{ok, #port_allocation{service = Service}}, _} <- 
                                  lists:zip(Results, Services)],
            {_CleanupReply, CleanupState} = do_release_ports(SuccessfulServices, State),
            
            Errors = [{Service, Error} || {{error, Error}, Service} <- 
                     lists:zip(Results, Services)],
            {{error, {allocation_failed, Errors}}, CleanupState}
    end.

allocate_single_port(Service, ServiceConfig, State) ->
    PreferredPort = maps:get(preferred_port, ServiceConfig, get_default_port(Service)),
    PortRange = maps:get(port_range, ServiceConfig, get_default_range(Service)),
    
    % Validate port range before attempting allocation
    case validate_port_range(PortRange) of
        ok ->
            % Validate preferred port is in valid range
            case validate_port(PreferredPort) of
                ok ->
                    allocate_with_retry(Service, PreferredPort, PortRange, 1);
                {error, Reason} ->
                    error_logger:error_msg("Invalid preferred port ~p for service ~p: ~p~n", 
                                         [PreferredPort, Service, Reason]),
                    {error, {invalid_preferred_port, PreferredPort, Reason}}
            end;
        {error, Reason} ->
            error_logger:error_msg("Invalid port range ~p for service ~p: ~p~n", 
                                 [PortRange, Service, Reason]),
            {error, {invalid_port_range, PortRange, Reason}}
    end.

allocate_with_retry(Service, PreferredPort, {RangeStart, RangeEnd}, Attempts) ->
    case port_registry:find_available_port({PreferredPort, {RangeStart, RangeEnd}}) of
        {ok, Port} ->
            case attempt_bind_port(Port) of
                ok ->
                    Allocation = #port_allocation{
                        service = Service,
                        port = Port,
                        pid = undefined,
                        status = bound,
                        allocated_at = erlang:system_time(second),
                        bind_attempts = Attempts,
                        last_error = undefined
                    },
                    port_registry:register_port(Port, Service),
                    error_logger:info_msg("Port ~p allocated for service ~p~n", [Port, Service]),
                    {ok, Allocation};
                {error, port_in_use} ->
                    % Port conflict detected - log warning and try next port
                    error_logger:warning_msg("Port conflict detected for service ~p on port ~p, trying next available port~n", 
                                           [Service, Port]),
                    % Mark this port as temporarily unavailable and try again
                    case Attempts < (RangeEnd - RangeStart + 1) of
                        true ->
                            % Try next port by excluding the failed one
                            NextPreferred = case Port + 1 =< RangeEnd of
                                true -> Port + 1;
                                false -> RangeStart
                            end,
                            allocate_with_retry(Service, NextPreferred, {RangeStart, RangeEnd}, Attempts + 1);
                        false ->
                            % Calculate how many ports were actually tried
                            TotalPortsInRange = RangeEnd - RangeStart + 1,
                            error_logger:error_msg("Port range {~p,~p} exhausted for service ~p after ~p attempts. "
                                                 "All ~p ports in range have been tried and are unavailable.~n", 
                                                 [RangeStart, RangeEnd, Service, Attempts, TotalPortsInRange]),
                            {error, {no_ports_available, {RangeStart, RangeEnd}, 
                                   {exhausted_after_attempts, Attempts, TotalPortsInRange}}}
                    end;
                {error, Reason} ->
                    error_logger:warning_msg("Failed to bind port ~p for service ~p: ~p~n", 
                                           [Port, Service, Reason]),
                    {error, {bind_failed, Port, Reason}}
            end;
        {error, Reason} ->
            TotalPortsInRange = RangeEnd - RangeStart + 1,
            error_logger:error_msg("No available port for service ~p in range {~p,~p}: ~p. "
                                 "Range contains ~p ports total.~n", 
                                 [Service, RangeStart, RangeEnd, Reason, TotalPortsInRange]),
            {error, {no_ports_available, {RangeStart, RangeEnd}, {registry_error, Reason, TotalPortsInRange}}}
    end.

attempt_bind_port(Port) ->
    case gen_tcp:listen(Port, [{reuseaddr, false}, {active, false}]) of
        {ok, Socket} ->
            gen_tcp:close(Socket),
            ok;
        {error, eaddrinuse} ->
            {error, port_in_use};
        {error, Reason} ->
            {error, Reason}
    end.

do_release_ports(Services, State) ->
    Allocations = State#state.allocations,
    {ReleasedServices, NewAllocations} = lists:foldl(fun(Service, {Released, Acc}) ->
        case maps:get(Service, Acc, undefined) of
            undefined ->
                {Released, Acc};
            Allocation ->
                Port = Allocation#port_allocation.port,
                port_registry:release_port(Port),
                error_logger:info_msg("Port ~p released for service ~p~n", [Port, Service]),
                {[Service | Released], maps:remove(Service, Acc)}
        end
    end, {[], Allocations}, Services),
    
    NewState = State#state{allocations = NewAllocations},
    {{ok, ReleasedServices}, NewState}.

do_force_restart_service(Service, State) ->
    % First release the port if allocated
    {_ReleaseReply, StateAfterRelease} = do_release_ports([Service], State),
    
    % Then try to allocate again
    {AllocateReply, FinalState} = do_allocate_ports([Service], StateAfterRelease),
    
    case AllocateReply of
        {ok, _PortInfo} ->
            {{ok, restarted}, FinalState};
        {error, Reason} ->
            {{error, {restart_failed, Reason}}, FinalState}
    end.

get_status_info(State) ->
    Allocations = State#state.allocations,
    maps:fold(fun(Service, Allocation, Acc) ->
        Info = #{
            service => Service,
            port => Allocation#port_allocation.port,
            status => Allocation#port_allocation.status,
            allocated_at => Allocation#port_allocation.allocated_at,
            bind_attempts => Allocation#port_allocation.bind_attempts
        },
        [Info | Acc]
    end, [], Allocations).

extract_port_info(Results) ->
    [#{service => Allocation#port_allocation.service,
       port => Allocation#port_allocation.port,
       status => Allocation#port_allocation.status} 
     || {ok, Allocation} <- Results].

get_default_config(Service) ->
    #{
        preferred_port => get_default_port(Service),
        port_range => get_default_range(Service),
        required => true
    }.

get_default_port(mcp_server) -> 8080;
get_default_port(oauth_server) -> 8081;
get_default_port(rest_api_server) -> 8082.

get_default_range(mcp_server) -> {8080, 8090};
get_default_range(oauth_server) -> {8081, 8091};
get_default_range(rest_api_server) -> {8082, 8092}.

%% Port validation functions
validate_port(Port) when is_integer(Port) ->
    if
        Port < 1024 ->
            {error, {port_too_low, Port, 1024}};
        Port > 65535 ->
            {error, {port_too_high, Port, 65535}};
        true ->
            ok
    end;
validate_port(Port) ->
    {error, {invalid_port_type, Port}}.

validate_port_range({Start, End}) when is_integer(Start), is_integer(End) ->
    case validate_port(Start) of
        ok ->
            case validate_port(End) of
                ok ->
                    if
                        Start > End ->
                            {error, {invalid_range_order, Start, End}};
                        true ->
                            ok
                    end;
                {error, Reason} ->
                    {error, {invalid_range_end, Reason}}
            end;
        {error, Reason} ->
            {error, {invalid_range_start, Reason}}
    end;
validate_port_range(Range) ->
    {error, {invalid_range_format, Range}}.
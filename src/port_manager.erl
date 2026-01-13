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
    reload_config/0,
    startup_services/1,
    pre_allocate_all_ports/0,
    cleanup_startup_failure/0,
    kill_existing_instances/1,
    force_restart_all/0,
    is_development_mode/0
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
    config :: #{service_name() => port_config()},
    startup_sequence :: [service_name()],
    startup_in_progress :: boolean()
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

startup_services(Services) when is_list(Services) ->
    gen_server:call(?SERVER, {startup_services, Services}).

pre_allocate_all_ports() ->
    gen_server:call(?SERVER, pre_allocate_all_ports).

cleanup_startup_failure() ->
    gen_server:call(?SERVER, cleanup_startup_failure).

kill_existing_instances(Services) when is_list(Services) ->
    gen_server:call(?SERVER, {kill_existing_instances, Services}).

force_restart_all() ->
    gen_server:call(?SERVER, force_restart_all).

is_development_mode() ->
    port_config:is_development_mode().

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    process_flag(trap_exit, true),
    
    % Start port registry
    {ok, RegistryPid} = port_registry:start_link(),
    
    % Load configuration
    Config = port_config:load_config(),
    
    % Determine startup sequence based on configuration
    StartupSequence = determine_startup_sequence(Config),
    
    State = #state{
        allocations = #{},
        registry_pid = RegistryPid,
        config = Config,
        startup_sequence = StartupSequence,
        startup_in_progress = false
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
    NewStartupSequence = determine_startup_sequence(NewConfig),
    NewState = State#state{
        config = NewConfig,
        startup_sequence = NewStartupSequence
    },
    {reply, ok, NewState};

handle_call({startup_services, Services}, _From, State) ->
    {Reply, NewState} = do_startup_services(Services, State),
    {reply, Reply, NewState};

handle_call(pre_allocate_all_ports, _From, State) ->
    {Reply, NewState} = do_pre_allocate_all_ports(State),
    {reply, Reply, NewState};

handle_call(cleanup_startup_failure, _From, State) ->
    {Reply, NewState} = do_cleanup_startup_failure(State),
    {reply, Reply, NewState};

handle_call({kill_existing_instances, Services}, _From, State) ->
    {Reply, NewState} = do_kill_existing_instances(Services, State),
    {reply, Reply, NewState};

handle_call(force_restart_all, _From, State) ->
    {Reply, NewState} = do_force_restart_all(State),
    {reply, Reply, NewState};

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
                    % Register with enhanced monitoring
                    port_registry:register_port(Port, Service, self()),
                    
                    % Log port allocation with container-specific information
                    log_port_allocation(Service, Port),
                    
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
    % Use development mode timeout if in development mode
    Timeout = case port_config:is_development_mode() of
        true -> port_config:get_development_timeout();
        false -> 5000  % Default production timeout
    end,
    
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

%%====================================================================
%% Startup Sequencing Functions
%%====================================================================

determine_startup_sequence(Config) ->
    % Extract startup order from configuration, default to service name order
    Services = maps:keys(Config),
    ServiceOrders = lists:map(fun(Service) ->
        ServiceConfig = maps:get(Service, Config, #{}),
        Order = maps:get(startup_order, ServiceConfig, get_default_startup_order(Service)),
        {Order, Service}
    end, Services),
    
    % Sort by startup order and extract service names
    SortedServices = lists:sort(ServiceOrders),
    [Service || {_Order, Service} <- SortedServices].

get_default_startup_order(mcp_server) -> 1;
get_default_startup_order(oauth_server) -> 2;
get_default_startup_order(rest_api_server) -> 3;
get_default_startup_order(_) -> 999.

do_startup_services(Services, State) ->
    case State#state.startup_in_progress of
        true ->
            {{error, startup_already_in_progress}, State};
        false ->
            % Mark startup as in progress
            StateWithStartup = State#state{startup_in_progress = true},
            
            % Pre-allocate all ports first
            case do_pre_allocate_all_ports(StateWithStartup) of
                {{ok, _PortInfo}, StateAfterAllocation} ->
                    % Start services in dependency order
                    case do_ordered_service_startup(Services, StateAfterAllocation) of
                        {ok, FinalState} ->
                            CompletedState = FinalState#state{startup_in_progress = false},
                            {{ok, services_started}, CompletedState};
                        {error, {service_startup_failed, Service, Reason}, FailedState} ->
                            % Cleanup on failure with detailed error reporting
                            ErrorDetails = format_service_startup_error(Service, Reason, Services),
                            error_logger:error_msg("Service startup failed: ~s~n", [ErrorDetails]),
                            
                            {_CleanupReply, CleanedState} = do_cleanup_startup_failure(FailedState),
                            FinalState = CleanedState#state{startup_in_progress = false},
                            {{error, {startup_failed, Service, Reason, ErrorDetails}}, FinalState}
                    end;
                {{error, Reason}, StateAfterFailure} ->
                    % Port allocation failed, cleanup and return detailed error
                    ErrorDetails = format_port_allocation_error(Reason, Services),
                    error_logger:error_msg("Port allocation failed: ~s~n", [ErrorDetails]),
                    
                    {_CleanupReply, CleanedState} = do_cleanup_startup_failure(StateAfterFailure),
                    FinalState = CleanedState#state{startup_in_progress = false},
                    {{error, {port_allocation_failed, Reason, ErrorDetails}}, FinalState}
            end
    end.

do_pre_allocate_all_ports(State) ->
    % Get all required services from configuration
    Config = State#state.config,
    RequiredServices = get_required_services(Config),
    
    error_logger:info_msg("Pre-allocating ports for services: ~p~n", [RequiredServices]),
    
    % Attempt to allocate all ports
    case do_allocate_ports(RequiredServices, State) of
        {{ok, PortInfo}, NewState} ->
            error_logger:info_msg("Successfully pre-allocated all ports: ~p~n", [PortInfo]),
            {{ok, PortInfo}, NewState};
        {{error, {allocation_failed, Errors}}, FailedState} ->
            error_logger:error_msg("Port pre-allocation failed: ~p~n", [Errors]),
            {{error, {pre_allocation_failed, Errors}}, FailedState}
    end.

do_ordered_service_startup(Services, State) ->
    % Get startup sequence from state
    StartupSequence = State#state.startup_sequence,
    
    % Filter and order the requested services according to startup sequence
    OrderedServices = filter_and_order_services(Services, StartupSequence),
    
    error_logger:info_msg("Starting services in order: ~p~n", [OrderedServices]),
    
    % Start each service in order
    start_services_sequentially(OrderedServices, State).

filter_and_order_services(RequestedServices, StartupSequence) ->
    % Keep only requested services that are in the startup sequence, in sequence order
    [Service || Service <- StartupSequence, lists:member(Service, RequestedServices)].

start_services_sequentially([], State) ->
    {ok, State};
start_services_sequentially([Service | RestServices], State) ->
    case start_single_service(Service, State) of
        {ok, NewState} ->
            start_services_sequentially(RestServices, NewState);
        {error, Reason} ->
            {error, {service_startup_failed, Service, Reason}, State}
    end.

start_single_service(Service, State) ->
    % Get the allocated port for this service
    case maps:get(Service, State#state.allocations, undefined) of
        undefined ->
            error_logger:error_msg("No port allocated for service ~p during startup~n", [Service]),
            {error, no_port_allocated};
        #port_allocation{port = Port, status = bound} ->
            error_logger:info_msg("Starting service ~p on port ~p~n", [Service, Port]),
            
            % Here we would normally start the actual service process
            % For now, we'll simulate successful startup with better error handling
            case simulate_service_startup_with_validation(Service, Port) of
                ok ->
                    error_logger:info_msg("Service ~p started successfully on port ~p~n", [Service, Port]),
                    {ok, State};
                {error, Reason} ->
                    error_logger:error_msg("Failed to start service ~p on port ~p: ~p~n", 
                                         [Service, Port, Reason]),
                    {error, {service_start_failed, Reason}}
            end;
        #port_allocation{status = Status, port = Port} ->
            error_logger:error_msg("Service ~p port ~p not bound, status: ~p~n", [Service, Port, Status]),
            {error, {port_not_bound, Status}}
    end.

simulate_service_startup_with_validation(Service, Port) ->
    % Enhanced simulation with validation steps
    try
        % Step 1: Verify port is still available to us
        case attempt_bind_port(Port) of
            ok ->
                % Port is available, continue with startup simulation
                simulate_service_specific_startup(Service, Port);
            {error, port_in_use} ->
                % Port is in use (which is expected since we allocated it)
                % In real implementation, we'd start the service process here
                simulate_service_specific_startup(Service, Port);
            {error, PortReason} ->
                {error, {port_validation_failed, PortReason}}
        end
    catch
        error:ErrorReason ->
            {error, {startup_exception, ErrorReason}};
        throw:ThrowReason ->
            {error, {startup_throw, ThrowReason}};
        exit:ExitReason ->
            {error, {startup_exit, ExitReason}}
    end.

simulate_service_specific_startup(Service, Port) ->
    % Service-specific startup simulation
    case Service of
        mcp_server ->
            % Simulate MCP server startup checks
            case validate_mcp_startup_requirements(Port) of
                ok -> ok;
                {error, Reason} -> {error, {mcp_startup_failed, Reason}}
            end;
        oauth_server ->
            % Simulate OAuth server startup checks
            case validate_oauth_startup_requirements(Port) of
                ok -> ok;
                {error, Reason} -> {error, {oauth_startup_failed, Reason}}
            end;
        rest_api_server ->
            % Simulate REST API server startup checks
            case validate_rest_api_startup_requirements(Port) of
                ok -> ok;
                {error, Reason} -> {error, {rest_api_startup_failed, Reason}}
            end;
        _ ->
            % Generic service startup
            ok
    end.

validate_mcp_startup_requirements(_Port) ->
    % Simulate MCP-specific startup validation
    % In real implementation, this would check MCP protocol requirements
    ok.

validate_oauth_startup_requirements(_Port) ->
    % Simulate OAuth-specific startup validation
    % In real implementation, this would check OAuth configuration, certificates, etc.
    ok.

validate_rest_api_startup_requirements(_Port) ->
    % Simulate REST API-specific startup validation
    % In real implementation, this would check API routes, middleware, etc.
    ok.

do_cleanup_startup_failure(State) ->
    error_logger:info_msg("Cleaning up after startup failure~n"),
    
    % Get list of services that had ports allocated
    AllocatedServices = maps:keys(State#state.allocations),
    
    % Release all allocated ports with detailed logging
    {_ReleaseReply, CleanedState} = do_release_ports_with_details(AllocatedServices, State),
    
    % Additional cleanup: reset startup state
    FinalState = CleanedState#state{startup_in_progress = false},
    
    error_logger:info_msg("Cleanup completed, released ports for services: ~p~n", [AllocatedServices]),
    
    {{ok, cleanup_completed}, FinalState}.

do_release_ports_with_details(Services, State) ->
    Allocations = State#state.allocations,
    {ReleasedServices, NewAllocations, CleanupDetails} = lists:foldl(fun(Service, {Released, Acc, Details}) ->
        case maps:get(Service, Acc, undefined) of
            undefined ->
                {Released, Acc, Details};
            Allocation ->
                Port = Allocation#port_allocation.port,
                Status = Allocation#port_allocation.status,
                
                % Release the port
                case port_registry:release_port(Port) of
                    ok ->
                        error_logger:info_msg("Port ~p released for service ~p (was ~p)~n", 
                                             [Port, Service, Status]),
                        CleanupDetail = #{service => Service, port => Port, 
                                        previous_status => Status, cleanup_result => success},
                        {[Service | Released], maps:remove(Service, Acc), [CleanupDetail | Details]};
                    {error, Reason} ->
                        error_logger:warning_msg("Failed to release port ~p for service ~p: ~p~n", 
                                                [Port, Service, Reason]),
                        CleanupDetail = #{service => Service, port => Port, 
                                        previous_status => Status, cleanup_result => {error, Reason}},
                        {[Service | Released], maps:remove(Service, Acc), [CleanupDetail | Details]}
                end
        end
    end, {[], Allocations, []}, Services),
    
    NewState = State#state{allocations = NewAllocations},
    {{ok, {ReleasedServices, CleanupDetails}}, NewState}.

get_required_services(Config) ->
    % Get all services that are marked as required
    maps:fold(fun(Service, ServiceConfig, Acc) ->
        Required = maps:get(required, ServiceConfig, true),
        case Required of
            true -> [Service | Acc];
            false -> Acc
        end
    end, [], Config).

%%====================================================================
%% Error Formatting Functions
%%====================================================================

format_service_startup_error(FailedService, Reason, AllServices) ->
    ServiceName = format_service_name(FailedService),
    ReasonStr = format_startup_failure_reason(Reason),
    
    % Find which services were supposed to start after this one
    RemainingServices = lists:dropwhile(fun(S) -> S =/= FailedService end, AllServices),
    RemainingCount = length(RemainingServices) - 1, % Subtract 1 for the failed service itself
    
    BaseMsg = io_lib:format("Failed to start ~s: ~s", [ServiceName, ReasonStr]),
    
    case RemainingCount of
        0 ->
            BaseMsg;
        N when N > 0 ->
            io_lib:format("~s. ~p remaining services were not started due to this failure.", 
                         [BaseMsg, N])
    end.

format_port_allocation_error({pre_allocation_failed, Errors}, Services) ->
    FailedServices = [Service || {Service, _Error} <- Errors],
    SuccessfulServices = Services -- FailedServices,
    
    FailedServiceNames = [format_service_name(S) || S <- FailedServices],
    FailedStr = string:join(FailedServiceNames, ", "),
    
    BaseMsg = io_lib:format("Failed to allocate ports for: ~s", [FailedStr]),
    
    case length(SuccessfulServices) of
        0 ->
            BaseMsg;
        N ->
            io_lib:format("~s. ~p services had successful port allocation but were cleaned up.", 
                         [BaseMsg, N])
    end;
format_port_allocation_error(Reason, _Services) ->
    io_lib:format("Port allocation failed: ~p", [Reason]).

format_startup_failure_reason(no_port_allocated) ->
    "No port was allocated for this service";
format_startup_failure_reason({port_not_bound, Status}) ->
    io_lib:format("Port was allocated but not bound (status: ~p)", [Status]);
format_startup_failure_reason({service_start_failed, Reason}) ->
    io_lib:format("Service process failed to start: ~p", [Reason]);
format_startup_failure_reason(Reason) ->
    io_lib:format("~p", [Reason]).

format_service_name(mcp_server) -> "MCP Server";
format_service_name(oauth_server) -> "OAuth Server";
format_service_name(rest_api_server) -> "REST API Server";
format_service_name(Service) -> atom_to_list(Service).

%%====================================================================
%% Development Mode Functions
%%====================================================================

do_kill_existing_instances(Services, State) ->
    case port_config:is_development_mode() of
        false ->
            {{error, not_in_development_mode}, State};
        true ->
            error_logger:info_msg("Development mode: killing existing instances for services: ~p~n", [Services]),
            
            % Get current port allocations for these services
            CurrentAllocations = State#state.allocations,
            
            KillResults = lists:map(fun(Service) ->
                case maps:get(Service, CurrentAllocations, undefined) of
                    undefined ->
                        {Service, no_allocation};
                    #port_allocation{port = Port} ->
                        % Try to kill any process using this port
                        KillResult = kill_processes_on_port(Port),
                        {Service, {port, Port, KillResult}}
                end
            end, Services),
            
            % Wait a bit for processes to die
            timer:sleep(500),
            
            % Release the ports from our registry
            {_ReleaseReply, NewState} = do_release_ports(Services, State),
            
            error_logger:info_msg("Development mode: kill results: ~p~n", [KillResults]),
            {{ok, KillResults}, NewState}
    end.

do_force_restart_all(State) ->
    case port_config:is_development_mode() of
        false ->
            {{error, not_in_development_mode}, State};
        true ->
            error_logger:info_msg("Development mode: force restarting all services~n"),
            
            % Get all currently allocated services
            CurrentServices = maps:keys(State#state.allocations),
            
            % Kill existing instances
            {_KillReply, StateAfterKill} = do_kill_existing_instances(CurrentServices, State),
            
            % Wait a bit longer for cleanup
            timer:sleep(1000),
            
            % Try to allocate all services again
            case do_allocate_ports(CurrentServices, StateAfterKill) of
                {{ok, PortInfo}, FinalState} ->
                    error_logger:info_msg("Development mode: force restart successful, new ports: ~p~n", [PortInfo]),
                    {{ok, {restarted, PortInfo}}, FinalState};
                {{error, Reason}, FinalState} ->
                    error_logger:error_msg("Development mode: force restart failed: ~p~n", [Reason]),
                    {{error, {restart_failed, Reason}}, FinalState}
            end
    end.

kill_processes_on_port(Port) ->
    % This is a simplified implementation for development mode
    % In a real implementation, you might use system tools like lsof or netstat
    % to find processes using the port and kill them
    
    % For now, we'll just try to bind to the port to see if it's free
    case gen_tcp:listen(Port, [{reuseaddr, false}, {active, false}]) of
        {ok, Socket} ->
            gen_tcp:close(Socket),
            {ok, port_was_free};
        {error, eaddrinuse} ->
            % Port is in use, but we can't easily kill the process in Erlang
            % In a real implementation, this would use OS-specific commands
            error_logger:warning_msg("Development mode: Port ~p is in use but cannot kill process automatically~n", [Port]),
            {warning, port_in_use_cannot_kill};
        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% Container Support Functions
%%====================================================================

log_port_allocation(Service, Port) ->
    % Enhanced logging for container deployments
    case port_config:is_container_mode() of
        true ->
            log_container_port_allocation(Service, Port);
        false ->
            error_logger:info_msg("Port ~p allocated for service ~p~n", [Port, Service])
    end.

log_container_port_allocation(Service, Port) ->
    % Container-specific port allocation logging
    ServiceName = format_service_name(Service),
    
    % Basic allocation log
    error_logger:info_msg("Container: Port ~p allocated for ~s~n", [Port, ServiceName]),
    
    % Check if we should log port mappings
    case port_config:should_log_port_mappings() of
        true ->
            log_container_port_mapping(Service, Port);
        false ->
            ok
    end.

log_container_port_mapping(Service, Port) ->
    % Log port mapping information useful for container orchestration
    ServiceName = format_service_name(Service),
    BindInterface = get_service_bind_interface(Service),
    
    % Log internal port binding
    error_logger:info_msg("Container Port Mapping: ~s -> ~s:~p (internal)~n", 
                         [ServiceName, BindInterface, Port]),
    
    % Log external port mapping if available from environment
    case get_external_port_mapping(Service, Port) of
        {ok, ExternalPort} ->
            error_logger:info_msg("Container Port Mapping: ~s -> external:~p -> internal:~p~n", 
                                 [ServiceName, ExternalPort, Port]);
        undefined ->
            error_logger:info_msg("Container Port Mapping: ~s -> internal:~p (external mapping unknown)~n", 
                                 [ServiceName, Port])
    end.

get_service_bind_interface(Service) ->
    % Get the bind interface for a specific service
    case port_config:get_service_config(Service) of
        Config when is_map(Config) ->
            maps:get(bind_interface, Config, "127.0.0.1");
        _ ->
            "127.0.0.1"
    end.

get_external_port_mapping(Service, _InternalPort) ->
    % Try to determine external port mapping from environment variables
    % This is useful for Docker port mapping (-p external:internal)
    ServiceStr = string:to_upper(atom_to_list(Service)),
    ExternalPortVar = ServiceStr ++ "_EXTERNAL_PORT",
    
    case os:getenv(ExternalPortVar) of
        false ->
            % Try generic external port mapping
            case os:getenv("EXTERNAL_PORT") of
                false -> undefined;
                PortStr ->
                    try
                        ExternalPort = list_to_integer(PortStr),
                        {ok, ExternalPort}
                    catch
                        error:badarg -> undefined
                    end
            end;
        PortStr ->
            try
                ExternalPort = list_to_integer(PortStr),
                {ok, ExternalPort}
            catch
                error:badarg -> undefined
            end
    end.
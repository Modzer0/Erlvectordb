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

-module(port_registry).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    register_port/2,
    register_port/3,
    release_port/1,
    is_port_available/1,
    find_available_port/1,
    get_all_allocations/0,
    monitor_service/2,
    unmonitor_service/1,
    get_service_ports/1,
    cleanup_dead_services/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Types
-type service_name() :: atom().
-type port_binding() :: #{
    port => integer(),
    service => service_name(),
    pid => pid(),
    bind_time => integer()
}.

%% Records
-record(port_binding, {
    port :: integer(),
    service :: service_name(),
    pid :: pid() | undefined,
    bind_time :: integer(),
    monitor_ref :: reference() | undefined
}).

-record(state, {
    bindings :: #{integer() => #port_binding{}},
    reserved_ports :: [integer()],
    service_monitors :: #{service_name() => {pid(), reference()}}
}).

-define(SERVER, ?MODULE).
-define(MIN_PORT, 1024).
-define(MAX_PORT, 65535).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

register_port(Port, Service) when is_integer(Port), is_atom(Service) ->
    gen_server:call(?SERVER, {register_port, Port, Service, undefined}).

register_port(Port, Service, Pid) when is_integer(Port), is_atom(Service) ->
    gen_server:call(?SERVER, {register_port, Port, Service, Pid}).

release_port(Port) when is_integer(Port) ->
    gen_server:call(?SERVER, {release_port, Port}).

is_port_available(Port) when is_integer(Port) ->
    gen_server:call(?SERVER, {is_port_available, Port}).

find_available_port({PreferredPort, {RangeStart, RangeEnd}}) ->
    gen_server:call(?SERVER, {find_available_port, PreferredPort, RangeStart, RangeEnd}).

get_all_allocations() ->
    gen_server:call(?SERVER, get_all_allocations).

monitor_service(Service, Pid) when is_atom(Service), is_pid(Pid) ->
    gen_server:call(?SERVER, {monitor_service, Service, Pid}).

unmonitor_service(Service) when is_atom(Service) ->
    gen_server:call(?SERVER, {unmonitor_service, Service}).

get_service_ports(Service) when is_atom(Service) ->
    gen_server:call(?SERVER, {get_service_ports, Service}).

cleanup_dead_services() ->
    gen_server:call(?SERVER, cleanup_dead_services).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    process_flag(trap_exit, true),
    State = #state{
        bindings = #{},
        reserved_ports = get_reserved_ports(),
        service_monitors = #{}
    },
    {ok, State}.

handle_call({register_port, Port, Service, Pid}, _From, State) ->
    case validate_port(Port) of
        ok ->
            case maps:is_key(Port, State#state.bindings) of
                true ->
                    {reply, {error, port_already_registered}, State};
                false ->
                    % Set up monitoring if Pid is provided
                    {MonitorRef, ActualPid} = case Pid of
                        undefined -> 
                            {undefined, undefined};
                        _ when is_pid(Pid) ->
                            Ref = monitor(process, Pid),
                            {Ref, Pid}
                    end,
                    
                    Binding = #port_binding{
                        port = Port,
                        service = Service,
                        pid = ActualPid,
                        bind_time = erlang:system_time(second),
                        monitor_ref = MonitorRef
                    },
                    NewBindings = maps:put(Port, Binding, State#state.bindings),
                    NewState = State#state{bindings = NewBindings},
                    
                    error_logger:info_msg("Port ~p registered for service ~p (pid: ~p)~n", 
                                         [Port, Service, ActualPid]),
                    {reply, ok, NewState}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({release_port, Port}, _From, State) ->
    case maps:get(Port, State#state.bindings, undefined) of
        undefined ->
            {reply, {error, port_not_registered}, State};
        Binding ->
            % Clean up monitoring if it exists
            case Binding#port_binding.monitor_ref of
                undefined -> ok;
                MonitorRef -> demonitor(MonitorRef, [flush])
            end,
            
            Service = Binding#port_binding.service,
            NewBindings = maps:remove(Port, State#state.bindings),
            NewState = State#state{bindings = NewBindings},
            
            error_logger:info_msg("Port ~p released for service ~p~n", [Port, Service]),
            {reply, ok, NewState}
    end;

handle_call({is_port_available, Port}, _From, State) ->
    Available = not maps:is_key(Port, State#state.bindings) andalso
                not lists:member(Port, State#state.reserved_ports) andalso
                is_port_bindable(Port),
    {reply, Available, State};

handle_call({find_available_port, PreferredPort, RangeStart, RangeEnd}, _From, State) ->
    Reply = do_find_available_port(PreferredPort, RangeStart, RangeEnd, State),
    {reply, Reply, State};

handle_call(get_all_allocations, _From, State) ->
    Allocations = maps:fold(fun(Port, Binding, Acc) ->
        Info = #{
            port => Port,
            service => Binding#port_binding.service,
            pid => Binding#port_binding.pid,
            bind_time => Binding#port_binding.bind_time,
            monitored => Binding#port_binding.monitor_ref =/= undefined
        },
        [Info | Acc]
    end, [], State#state.bindings),
    {reply, {ok, Allocations}, State};

handle_call({monitor_service, Service, Pid}, _From, State) ->
    % Set up monitoring for a service process
    case maps:get(Service, State#state.service_monitors, undefined) of
        undefined ->
            MonitorRef = monitor(process, Pid),
            NewMonitors = maps:put(Service, {Pid, MonitorRef}, State#state.service_monitors),
            NewState = State#state{service_monitors = NewMonitors},
            
            error_logger:info_msg("Started monitoring service ~p (pid: ~p)~n", [Service, Pid]),
            {reply, ok, NewState};
        {OldPid, OldRef} ->
            % Already monitoring, update to new process
            demonitor(OldRef, [flush]),
            MonitorRef = monitor(process, Pid),
            NewMonitors = maps:put(Service, {Pid, MonitorRef}, State#state.service_monitors),
            NewState = State#state{service_monitors = NewMonitors},
            
            error_logger:info_msg("Updated monitoring for service ~p from ~p to ~p~n", 
                                 [Service, OldPid, Pid]),
            {reply, ok, NewState}
    end;

handle_call({unmonitor_service, Service}, _From, State) ->
    case maps:get(Service, State#state.service_monitors, undefined) of
        undefined ->
            {reply, {error, service_not_monitored}, State};
        {Pid, MonitorRef} ->
            demonitor(MonitorRef, [flush]),
            NewMonitors = maps:remove(Service, State#state.service_monitors),
            NewState = State#state{service_monitors = NewMonitors},
            
            error_logger:info_msg("Stopped monitoring service ~p (pid: ~p)~n", [Service, Pid]),
            {reply, ok, NewState}
    end;

handle_call({get_service_ports, Service}, _From, State) ->
    ServicePorts = maps:fold(fun(Port, Binding, Acc) ->
        case Binding#port_binding.service of
            Service -> [Port | Acc];
            _ -> Acc
        end
    end, [], State#state.bindings),
    {reply, {ok, ServicePorts}, State};

handle_call(cleanup_dead_services, _From, State) ->
    {CleanedPorts, NewState} = cleanup_dead_processes(State),
    case CleanedPorts of
        [] ->
            {reply, {ok, no_cleanup_needed}, NewState};
        _ ->
            error_logger:info_msg("Cleaned up ports for dead services: ~p~n", [CleanedPorts]),
            {reply, {ok, {cleaned_ports, CleanedPorts}}, NewState}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', MonitorRef, process, Pid, Reason}, State) ->
    % Handle process death - clean up associated ports
    error_logger:info_msg("Monitored process ~p died with reason ~p, cleaning up ports~n", 
                         [Pid, Reason]),
    
    % Find and clean up ports associated with this process
    {CleanedPorts, NewBindings} = maps:fold(fun(Port, Binding, {Cleaned, Bindings}) ->
        case Binding#port_binding.monitor_ref of
            MonitorRef ->
                % This binding is associated with the dead process
                Service = Binding#port_binding.service,
                error_logger:info_msg("Cleaning up port ~p for dead service ~p~n", 
                                     [Port, Service]),
                {[{Port, Service} | Cleaned], Bindings};
            _ ->
                % Keep this binding
                {Cleaned, maps:put(Port, Binding, Bindings)}
        end
    end, {[], #{}}, State#state.bindings),
    
    % Also clean up service monitors
    NewMonitors = maps:filter(fun(_Service, {MonitoredPid, _Ref}) ->
        MonitoredPid =/= Pid
    end, State#state.service_monitors),
    
    NewState = State#state{
        bindings = NewBindings,
        service_monitors = NewMonitors
    },
    
    case CleanedPorts of
        [] ->
            error_logger:warning_msg("Process ~p died but no ports were associated with it~n", [Pid]);
        _ ->
            error_logger:info_msg("Cleaned up ~p ports due to process death: ~p~n", 
                                 [length(CleanedPorts), CleanedPorts])
    end,
    
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

validate_port(Port) when is_integer(Port) ->
    if
        Port < ?MIN_PORT ->
            {error, {port_too_low, Port, ?MIN_PORT}};
        Port > ?MAX_PORT ->
            {error, {port_too_high, Port, ?MAX_PORT}};
        true ->
            ok
    end;
validate_port(Port) ->
    {error, {invalid_port_type, Port}}.

is_port_bindable(Port) ->
    case gen_tcp:listen(Port, [{reuseaddr, false}, {active, false}]) of
        {ok, Socket} ->
            gen_tcp:close(Socket),
            true;
        {error, eaddrinuse} ->
            false;
        {error, _} ->
            false
    end.

do_find_available_port(PreferredPort, RangeStart, RangeEnd, State) ->
    % Validate range
    case validate_port_range(RangeStart, RangeEnd) of
        ok ->
            % Try preferred port first if it's in range
            case PreferredPort >= RangeStart andalso PreferredPort =< RangeEnd of
                true ->
                    case is_port_available_internal(PreferredPort, State) of
                        true ->
                            {ok, PreferredPort};
                        false ->
                            find_next_available_port(RangeStart, RangeEnd, State, PreferredPort)
                    end;
                false ->
                    find_next_available_port(RangeStart, RangeEnd, State, RangeStart)
            end;
        {error, Reason} ->
            {error, Reason}
    end.

validate_port_range(Start, End) when is_integer(Start), is_integer(End) ->
    if
        Start < ?MIN_PORT ->
            {error, {range_start_too_low, Start, ?MIN_PORT}};
        End > ?MAX_PORT ->
            {error, {range_end_too_high, End, ?MAX_PORT}};
        Start > End ->
            {error, {invalid_range, Start, End}};
        true ->
            ok
    end;
validate_port_range(Start, End) ->
    {error, {invalid_range_types, Start, End}}.

find_next_available_port(RangeStart, RangeEnd, State, StartFrom) ->
    % Try ports from StartFrom to RangeEnd
    case find_in_range(StartFrom, RangeEnd, State) of
        {ok, Port} ->
            {ok, Port};
        not_found ->
            % If StartFrom was not RangeStart, try from RangeStart to StartFrom-1
            case StartFrom > RangeStart of
                true ->
                    find_in_range(RangeStart, StartFrom - 1, State);
                false ->
                    {error, no_ports_available_in_range}
            end
    end.

find_in_range(Start, End, State) when Start =< End ->
    case is_port_available_internal(Start, State) of
        true ->
            {ok, Start};
        false ->
            find_in_range(Start + 1, End, State)
    end;
find_in_range(_Start, _End, _State) ->
    not_found.

is_port_available_internal(Port, State) ->
    not maps:is_key(Port, State#state.bindings) andalso
    not lists:member(Port, State#state.reserved_ports) andalso
    is_port_bindable(Port).

get_reserved_ports() ->
    % Common reserved ports that should not be used
    [22, 23, 25, 53, 80, 110, 143, 443, 993, 995].

%%====================================================================
%% Maintenance functions
%%====================================================================

cleanup_dead_processes(State) ->
    % Check all monitored processes and clean up dead ones
    {CleanedPorts, NewBindings} = maps:fold(fun(Port, Binding, {Cleaned, Bindings}) ->
        case Binding#port_binding.pid of
            undefined ->
                % No PID associated, keep the binding
                {Cleaned, maps:put(Port, Binding, Bindings)};
            Pid ->
                case is_process_alive(Pid) of
                    true ->
                        % Process is alive, keep the binding
                        {Cleaned, maps:put(Port, Binding, Bindings)};
                    false ->
                        % Process is dead, clean up
                        Service = Binding#port_binding.service,
                        error_logger:info_msg("Cleaning up port ~p for dead service ~p (pid: ~p)~n", 
                                             [Port, Service, Pid]),
                        
                        % Clean up monitor if it exists
                        case Binding#port_binding.monitor_ref of
                            undefined -> ok;
                            MonitorRef -> demonitor(MonitorRef, [flush])
                        end,
                        
                        {[{Port, Service} | Cleaned], Bindings}
                end
        end
    end, {[], #{}}, State#state.bindings),
    
    % Also clean up dead service monitors
    NewMonitors = maps:filter(fun(_Service, {Pid, MonitorRef}) ->
        case is_process_alive(Pid) of
            true -> 
                true;
            false ->
                demonitor(MonitorRef, [flush]),
                false
        end
    end, State#state.service_monitors),
    
    NewState = State#state{
        bindings = NewBindings,
        service_monitors = NewMonitors
    },
    
    {CleanedPorts, NewState}.
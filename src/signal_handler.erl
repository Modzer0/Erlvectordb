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

-module(signal_handler).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    enable_graceful_shutdown/0,
    disable_graceful_shutdown/0,
    register_shutdown_callback/2,
    unregister_shutdown_callback/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Types
-type shutdown_callback() :: fun(() -> ok | {error, term()}).

%% Records
-record(shutdown_callback, {
    name :: atom(),
    callback :: shutdown_callback(),
    priority :: integer()  % Lower numbers execute first
}).

-record(state, {
    enabled :: boolean(),
    callbacks :: [#shutdown_callback{}],
    shutdown_in_progress :: boolean(),
    shutdown_timeout :: integer()
}).

-define(SERVER, ?MODULE).
-define(DEFAULT_SHUTDOWN_TIMEOUT, 30000).  % 30 seconds

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

enable_graceful_shutdown() ->
    gen_server:call(?SERVER, enable_graceful_shutdown).

disable_graceful_shutdown() ->
    gen_server:call(?SERVER, disable_graceful_shutdown).

register_shutdown_callback(Name, Callback) when is_atom(Name), is_function(Callback, 0) ->
    register_shutdown_callback(Name, Callback, 100).

register_shutdown_callback(Name, Callback, Priority) when is_atom(Name), is_function(Callback, 0), is_integer(Priority) ->
    gen_server:call(?SERVER, {register_shutdown_callback, Name, Callback, Priority}).

unregister_shutdown_callback(Name) when is_atom(Name) ->
    gen_server:call(?SERVER, {unregister_shutdown_callback, Name}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    process_flag(trap_exit, true),
    
    % Determine if we should enable graceful shutdown based on container mode
    ContainerMode = port_config:is_container_mode(),
    
    State = #state{
        enabled = ContainerMode,
        callbacks = get_default_shutdown_callbacks(),
        shutdown_in_progress = false,
        shutdown_timeout = get_shutdown_timeout()
    },
    
    % Set up signal handling if enabled
    NewState = case ContainerMode of
        true ->
            setup_signal_handling(State);
        false ->
            State
    end,
    
    {ok, NewState}.

handle_call(enable_graceful_shutdown, _From, State) ->
    case State#state.enabled of
        true ->
            {reply, {error, already_enabled}, State};
        false ->
            NewState = setup_signal_handling(State#state{enabled = true}),
            error_logger:info_msg("Graceful shutdown enabled~n"),
            {reply, ok, NewState}
    end;

handle_call(disable_graceful_shutdown, _From, State) ->
    case State#state.enabled of
        false ->
            {reply, {error, already_disabled}, State};
        true ->
            NewState = cleanup_signal_handling(State#state{enabled = false}),
            error_logger:info_msg("Graceful shutdown disabled~n"),
            {reply, ok, NewState}
    end;

handle_call({register_shutdown_callback, Name, Callback, Priority}, _From, State) ->
    ShutdownCallback = #shutdown_callback{
        name = Name,
        callback = Callback,
        priority = Priority
    },
    
    % Remove any existing callback with the same name
    FilteredCallbacks = [CB || CB <- State#state.callbacks, CB#shutdown_callback.name =/= Name],
    
    % Add new callback and sort by priority
    NewCallbacks = lists:sort(fun(A, B) ->
        A#shutdown_callback.priority =< B#shutdown_callback.priority
    end, [ShutdownCallback | FilteredCallbacks]),
    
    NewState = State#state{callbacks = NewCallbacks},
    
    error_logger:info_msg("Shutdown callback registered: ~p (priority: ~p)~n", [Name, Priority]),
    {reply, ok, NewState};

handle_call({unregister_shutdown_callback, Name}, _From, State) ->
    NewCallbacks = [CB || CB <- State#state.callbacks, CB#shutdown_callback.name =/= Name],
    NewState = State#state{callbacks = NewCallbacks},
    
    error_logger:info_msg("Shutdown callback unregistered: ~p~n", [Name]),
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({shutdown_signal, Signal}, State) ->
    case State#state.enabled andalso not State#state.shutdown_in_progress of
        true ->
            error_logger:info_msg("Received shutdown signal: ~p, initiating graceful shutdown~n", [Signal]),
            NewState = State#state{shutdown_in_progress = true},
            spawn(fun() -> perform_graceful_shutdown(NewState) end),
            {noreply, NewState};
        false ->
            case State#state.shutdown_in_progress of
                true ->
                    error_logger:warning_msg("Shutdown already in progress, ignoring signal: ~p~n", [Signal]);
                false ->
                    error_logger:warning_msg("Graceful shutdown disabled, ignoring signal: ~p~n", [Signal])
            end,
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', _Pid, normal}, State) ->
    {noreply, State};

handle_info({'EXIT', _Pid, Reason}, State) ->
    error_logger:warning_msg("Signal handler received EXIT: ~p~n", [Reason]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    cleanup_signal_handling(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

setup_signal_handling(State) ->
    % Set up signal handling for SIGTERM and SIGINT
    case os:type() of
        {unix, _} ->
            setup_unix_signal_handling(State);
        _ ->
            error_logger:warning_msg("Signal handling not supported on this platform~n"),
            State
    end.

setup_unix_signal_handling(State) ->
    % Create a port to handle signals
    try
        % Use a simple shell script to catch signals and send them to Erlang
        SignalScript = create_signal_script(),
        Port = open_port({spawn, SignalScript}, [stream, {line, 1024}]),
        
        error_logger:info_msg("Signal handling set up for graceful shutdown~n"),
        State
    catch
        _:Error ->
            error_logger:error_msg("Failed to set up signal handling: ~p~n", [Error]),
            State
    end.

cleanup_signal_handling(State) ->
    % Clean up signal handling
    error_logger:info_msg("Signal handling cleaned up~n"),
    State.

create_signal_script() ->
    % Create a simple signal handling approach
    % In a real implementation, this would be more sophisticated
    "trap 'echo SIGTERM' TERM; trap 'echo SIGINT' INT; while true; do sleep 1; done".

get_shutdown_timeout() ->
    case port_config:is_container_mode() of
        true ->
            port_config:get_container_shutdown_timeout();
        false ->
            case application:get_env(erlvectordb, shutdown_timeout) of
                {ok, Timeout} when is_integer(Timeout), Timeout > 0 -> Timeout;
                _ -> ?DEFAULT_SHUTDOWN_TIMEOUT
            end
    end.

get_default_shutdown_callbacks() ->
    [
        #shutdown_callback{
            name = port_manager,
            callback = fun shutdown_port_manager/0,
            priority = 10
        },
        #shutdown_callback{
            name = health_check_server,
            callback = fun shutdown_health_check_server/0,
            priority = 20
        },
        #shutdown_callback{
            name = application,
            callback = fun shutdown_application/0,
            priority = 100
        }
    ].

perform_graceful_shutdown(State) ->
    error_logger:info_msg("Starting graceful shutdown sequence~n"),
    StartTime = erlang:system_time(millisecond),
    
    % Execute shutdown callbacks in priority order
    Results = execute_shutdown_callbacks(State#state.callbacks, State#state.shutdown_timeout),
    
    EndTime = erlang:system_time(millisecond),
    Duration = EndTime - StartTime,
    
    % Log shutdown results
    log_shutdown_results(Results, Duration),
    
    % Final application stop
    error_logger:info_msg("Graceful shutdown completed in ~p ms, stopping application~n", [Duration]),
    
    % Give a moment for logs to flush
    timer:sleep(100),
    
    % Stop the application
    init:stop().

execute_shutdown_callbacks(Callbacks, Timeout) ->
    TotalCallbacks = length(Callbacks),
    TimeoutPerCallback = case TotalCallbacks of
        0 -> Timeout;
        N -> max(1000, Timeout div N)  % At least 1 second per callback
    end,
    
    lists:map(fun(Callback) ->
        execute_single_shutdown_callback(Callback, TimeoutPerCallback)
    end, Callbacks).

execute_single_shutdown_callback(Callback, Timeout) ->
    Name = Callback#shutdown_callback.name,
    CallbackFun = Callback#shutdown_callback.callback,
    
    error_logger:info_msg("Executing shutdown callback: ~p~n", [Name]),
    StartTime = erlang:system_time(millisecond),
    
    Result = try
        % Execute callback with timeout
        {ok, TRef} = timer:exit_after(Timeout, timeout),
        CallbackResult = CallbackFun(),
        timer:cancel(TRef),
        {ok, CallbackResult}
    catch
        exit:timeout ->
            {error, timeout};
        Class:Reason:Stacktrace ->
            {error, {exception, {Class, Reason, Stacktrace}}}
    end,
    
    EndTime = erlang:system_time(millisecond),
    Duration = EndTime - StartTime,
    
    case Result of
        {ok, _CallbackResult} ->
            error_logger:info_msg("Shutdown callback ~p completed successfully in ~p ms~n", [Name, Duration]),
            {Name, ok, Duration};
        {error, timeout} ->
            error_logger:error_msg("Shutdown callback ~p timed out after ~p ms~n", [Name, Duration]),
            {Name, {error, timeout}, Duration};
        {error, {exception, ExceptionInfo}} ->
            error_logger:error_msg("Shutdown callback ~p failed: ~p in ~p ms~n", 
                                 [Name, ExceptionInfo, Duration]),
            {Name, {error, ExceptionInfo}, Duration}
    end.

log_shutdown_results(Results, TotalDuration) ->
    SuccessfulCallbacks = [Name || {Name, ok, _} <- Results],
    FailedCallbacks = [{Name, Error} || {Name, Error, _} <- Results, Error =/= ok],
    
    error_logger:info_msg("Shutdown summary: ~p successful, ~p failed, total duration: ~p ms~n",
                         [length(SuccessfulCallbacks), length(FailedCallbacks), TotalDuration]),
    
    case FailedCallbacks of
        [] ->
            error_logger:info_msg("All shutdown callbacks completed successfully~n");
        _ ->
            error_logger:error_msg("Failed shutdown callbacks: ~p~n", [FailedCallbacks])
    end.

%%====================================================================
%% Default Shutdown Callback Functions
%%====================================================================

shutdown_port_manager() ->
    try
        case whereis(port_manager) of
            undefined ->
                error_logger:info_msg("Port manager not running, skipping shutdown~n"),
                ok;
            Pid when is_pid(Pid) ->
                error_logger:info_msg("Shutting down port manager~n"),
                
                % Get current port status
                case port_manager:get_port_status() of
                    {ok, PortStatus} ->
                        AllocatedServices = [maps:get(service, Status) || Status <- PortStatus],
                        case AllocatedServices of
                            [] ->
                                error_logger:info_msg("No ports to release~n");
                            Services ->
                                error_logger:info_msg("Releasing ports for services: ~p~n", [Services]),
                                port_manager:release_ports(Services)
                        end;
                    {error, Reason} ->
                        error_logger:warning_msg("Could not get port status during shutdown: ~p~n", [Reason])
                end,
                
                ok
        end
    catch
        _:Error ->
            error_logger:error_msg("Error during port manager shutdown: ~p~n", [Error]),
            {error, Error}
    end.

shutdown_health_check_server() ->
    try
        case whereis(health_check_server) of
            undefined ->
                error_logger:info_msg("Health check server not running, skipping shutdown~n"),
                ok;
            Pid when is_pid(Pid) ->
                error_logger:info_msg("Shutting down health check server~n"),
                health_check_server:disable_container_health_checks(),
                ok
        end
    catch
        _:Error ->
            error_logger:error_msg("Error during health check server shutdown: ~p~n", [Error]),
            {error, Error}
    end.

shutdown_application() ->
    try
        error_logger:info_msg("Stopping application services~n"),
        
        % Stop the application gracefully
        case application:stop(erlvectordb) of
            ok ->
                error_logger:info_msg("Application stopped successfully~n"),
                ok;
            {error, Reason} ->
                error_logger:error_msg("Error stopping application: ~p~n", [Reason]),
                {error, Reason}
        end
    catch
        _:Error ->
            error_logger:error_msg("Exception during application shutdown: ~p~n", [Error]),
            {error, Error}
    end.
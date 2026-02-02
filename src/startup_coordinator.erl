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

-module(startup_coordinator).
-behaviour(gen_server).

%% API
-export([start_link/0, coordinate_startup/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    startup_completed = false :: boolean()
}).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

coordinate_startup() ->
    gen_server:call(?SERVER, coordinate_startup, 30000).  % 30 second timeout

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    {ok, #state{}}.

handle_call(coordinate_startup, _From, State) ->
    case State#state.startup_completed of
        true ->
            {reply, {ok, already_completed}, State};
        false ->
            Result = do_coordinate_startup(),
            NewState = State#state{startup_completed = (element(1, Result) =:= ok)},
            {reply, Result, NewState}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

do_coordinate_startup() ->
    error_logger:info_msg("Starting coordinated application startup~n"),
    
    try
        % Step 1: Pre-allocate all required ports
        error_logger:info_msg("Step 1: Pre-allocating ports~n"),
        case port_manager:pre_allocate_all_ports() of
            {ok, PortInfo} ->
                error_logger:info_msg("Ports pre-allocated successfully: ~p~n", [PortInfo]),
                
                % Step 2: Start network services in dependency order
                error_logger:info_msg("Step 2: Starting network services~n"),
                NetworkServices = [mcp_server, oauth_server, rest_api_server],
                case start_network_services(NetworkServices) of
                    ok ->
                        error_logger:info_msg("All network services started successfully~n"),
                        {ok, startup_completed};
                    {error, {service_start_failed, Service, Reason}} ->
                        error_logger:error_msg("Failed to start service ~p: ~p~n", [Service, Reason]),
                        
                        % Cleanup on failure
                        cleanup_failed_startup(NetworkServices),
                        {error, {service_startup_failed, Service, Reason}};
                    {error, Reason} ->
                        error_logger:error_msg("Network service startup failed: ~p~n", [Reason]),
                        cleanup_failed_startup(NetworkServices),
                        {error, {network_services_failed, Reason}}
                end;
            {error, Reason} ->
                error_logger:error_msg("Port pre-allocation failed: ~p~n", [Reason]),
                {error, {port_allocation_failed, Reason}}
        end
    catch
        Class:Error:Stacktrace ->
            error_logger:error_msg("Startup coordination failed with exception: ~p:~p~n~p~n", 
                                 [Class, Error, Stacktrace]),
            {error, {startup_exception, Class, Error}}
    end.

start_network_services(Services) ->
    % Start services one by one, checking each one
    start_services_sequentially(Services).

start_services_sequentially([]) ->
    ok;
start_services_sequentially([Service | RestServices]) ->
    case start_single_network_service(Service) of
        ok ->
            start_services_sequentially(RestServices);
        {error, Reason} ->
            {error, {service_start_failed, Service, Reason}}
    end.

start_single_network_service(Service) ->
    error_logger:info_msg("Starting network service: ~p~n", [Service]),
    
    % For oauth_server, we need to check if the HTTP handler is listening, not just if the gen_server exists
    ShouldStart = case Service of
        oauth_server ->
            % Check if port is actually listening
            case port_manager:get_service_port(oauth_server) of
                {ok, Port} ->
                    case gen_tcp:connect("127.0.0.1", Port, [binary, {active, false}], 100) of
                        {ok, Socket} ->
                            gen_tcp:close(Socket),
                            false; % Already listening
                        {error, _} ->
                            true % Not listening, need to start
                    end;
                {error, _} ->
                    true % Can't check, try to start
            end;
        _ ->
            % For other services, check if process is registered
            whereis(Service) =:= undefined
    end,
    
    case ShouldStart of
        true ->
            % Service not running, start it
            case start_service_process(Service) of
                {ok, Pid} when is_pid(Pid) ->
                    error_logger:info_msg("Service ~p started with PID ~p~n", [Service, Pid]),
                    
                    % Verify service is accessible
                    case verify_service_startup(Service) of
                        ok ->
                            ok;
                        {error, Reason} ->
                            error_logger:error_msg("Service ~p startup verification failed: ~p~n", 
                                                 [Service, Reason]),
                            {error, {verification_failed, Reason}}
                    end;
                {ok, disabled} ->
                    error_logger:info_msg("Service ~p is disabled, skipping~n", [Service]),
                    ok;
                {error, Reason} ->
                    error_logger:error_msg("Failed to start service ~p: ~p~n", [Service, Reason]),
                    {error, {start_failed, Reason}}
            end;
        false ->
            error_logger:info_msg("Service ~p already running~n", [Service]),
            ok
    end.

start_service_process(mcp_server) ->
    mcp_server:start_link();
start_service_process(oauth_server) ->
    % OAuth server gen_server is already started by supervisor
    % We need to start the HTTP handler
    oauth_http_handler:start_link();
start_service_process(rest_api_server) ->
    case rest_api_server:start_link() of
        {ok, Pid} -> {ok, Pid};
        ignore -> 
            % REST API server is disabled, this is acceptable
            error_logger:info_msg("REST API server is disabled~n"),
            {ok, disabled};
        {error, Reason} -> {error, Reason}
    end;
start_service_process(Service) ->
    error_logger:warning_msg("Unknown service for startup: ~p~n", [Service]),
    {error, {unknown_service, Service}}.

verify_service_startup(Service) ->
    % Get the port for this service and verify it's accessible
    case port_manager:get_service_port(Service) of
        {ok, Port} ->
            error_logger:info_msg("Verifying service ~p on port ~p~n", [Service, Port]),
            
            % Try to connect to verify the service is listening
            case gen_tcp:connect("127.0.0.1", Port, [binary, {active, false}], 2000) of
                {ok, Socket} ->
                    gen_tcp:close(Socket),
                    error_logger:info_msg("Service ~p verified on port ~p~n", [Service, Port]),
                    ok;
                {error, econnrefused} ->
                    % Service might not be fully ready yet, give it a moment
                    timer:sleep(500),
                    case gen_tcp:connect("127.0.0.1", Port, [binary, {active, false}], 1000) of
                        {ok, Socket2} ->
                            gen_tcp:close(Socket2),
                            ok;
                        {error, econnrefused} ->
                            % Still not ready - this might be normal for some services
                            error_logger:warning_msg("Service ~p not accepting connections on port ~p (may be normal)~n", 
                                                    [Service, Port]),
                            ok;
                        {error, Reason} ->
                            {error, {connection_failed, Reason}}
                    end;
                {error, Reason} ->
                    error_logger:warning_msg("Could not verify service ~p on port ~p: ~p~n", 
                                           [Service, Port, Reason]),
                    % Don't fail startup for connection issues - service might be working
                    ok
            end;
        {error, service_not_allocated} ->
            error_logger:error_msg("Service ~p has no allocated port~n", [Service]),
            {error, no_port_allocated};
        {error, {service_not_bound, Status}} ->
            error_logger:error_msg("Service ~p port not bound, status: ~p~n", [Service, Status]),
            {error, {port_not_bound, Status}};
        {error, Reason} ->
            error_logger:error_msg("Failed to get port for service ~p: ~p~n", [Service, Reason]),
            {error, {port_query_failed, Reason}}
    end.

cleanup_failed_startup(Services) ->
    error_logger:info_msg("Cleaning up failed startup for services: ~p~n", [Services]),
    
    % Release any allocated ports
    port_manager:cleanup_startup_failure(),
    
    % Stop any started services
    lists:foreach(fun(Service) ->
        case whereis(Service) of
            undefined ->
                ok;
            Pid when is_pid(Pid) ->
                error_logger:info_msg("Stopping service ~p (PID: ~p)~n", [Service, Pid]),
                exit(Pid, shutdown)
        end
    end, Services),
    
    ok.
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

-module(health_check_server).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    get_health_status/0,
    register_health_check/2,
    unregister_health_check/1,
    enable_container_health_checks/0,
    disable_container_health_checks/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Types
-type health_status() :: healthy | unhealthy | degraded.
-type health_check_fun() :: fun(() -> {health_status(), term()}).

%% Records
-record(health_check, {
    name :: atom(),
    check_fun :: health_check_fun(),
    last_result :: {health_status(), term()} | undefined,
    last_check_time :: integer() | undefined
}).

-record(state, {
    checks :: #{atom() => #health_check{}},
    container_mode :: boolean(),
    health_check_port :: integer() | undefined,
    health_check_socket :: gen_tcp:socket() | undefined
}).

-define(SERVER, ?MODULE).
-define(DEFAULT_HEALTH_CHECK_PORT, 8090).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_health_status() ->
    gen_server:call(?SERVER, get_health_status).

register_health_check(Name, CheckFun) when is_atom(Name), is_function(CheckFun, 0) ->
    gen_server:call(?SERVER, {register_health_check, Name, CheckFun}).

unregister_health_check(Name) when is_atom(Name) ->
    gen_server:call(?SERVER, {unregister_health_check, Name}).

enable_container_health_checks() ->
    gen_server:call(?SERVER, enable_container_health_checks).

disable_container_health_checks() ->
    gen_server:call(?SERVER, disable_container_health_checks).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    process_flag(trap_exit, true),
    
    % Determine if we should enable container health checks
    ContainerMode = port_config:is_container_mode(),
    
    % Register default health checks
    DefaultChecks = get_default_health_checks(),
    
    State = #state{
        checks = DefaultChecks,
        container_mode = ContainerMode,
        health_check_port = undefined,
        health_check_socket = undefined
    },
    
    % Start health check endpoint if in container mode
    NewState = case ContainerMode of
        true ->
            start_health_check_endpoint(State);
        false ->
            State
    end,
    
    {ok, NewState}.

handle_call(get_health_status, _From, State) ->
    {OverallStatus, CheckResults} = run_all_health_checks(State),
    
    HealthInfo = #{
        overall_status => OverallStatus,
        checks => CheckResults,
        timestamp => erlang:system_time(second),
        container_mode => State#state.container_mode
    },
    
    {reply, {ok, HealthInfo}, State};

handle_call({register_health_check, Name, CheckFun}, _From, State) ->
    HealthCheck = #health_check{
        name = Name,
        check_fun = CheckFun,
        last_result = undefined,
        last_check_time = undefined
    },
    
    NewChecks = maps:put(Name, HealthCheck, State#state.checks),
    NewState = State#state{checks = NewChecks},
    
    error_logger:info_msg("Health check registered: ~p~n", [Name]),
    {reply, ok, NewState};

handle_call({unregister_health_check, Name}, _From, State) ->
    NewChecks = maps:remove(Name, State#state.checks),
    NewState = State#state{checks = NewChecks},
    
    error_logger:info_msg("Health check unregistered: ~p~n", [Name]),
    {reply, ok, NewState};

handle_call(enable_container_health_checks, _From, State) ->
    case State#state.container_mode of
        true ->
            {reply, {error, already_enabled}, State};
        false ->
            NewState = start_health_check_endpoint(State#state{container_mode = true}),
            {reply, ok, NewState}
    end;

handle_call(disable_container_health_checks, _From, State) ->
    case State#state.container_mode of
        false ->
            {reply, {error, already_disabled}, State};
        true ->
            NewState = stop_health_check_endpoint(State#state{container_mode = false}),
            {reply, ok, NewState}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, Socket, Data}, State) when Socket =:= State#state.health_check_socket ->
    % Handle health check HTTP request
    handle_health_check_request(Socket, Data, State),
    {noreply, State};

handle_info({tcp_closed, Socket}, State) when Socket =:= State#state.health_check_socket ->
    % Health check socket closed, restart if in container mode
    case State#state.container_mode of
        true ->
            error_logger:warning_msg("Health check socket closed, restarting~n"),
            NewState = start_health_check_endpoint(State#state{health_check_socket = undefined}),
            {noreply, NewState};
        false ->
            {noreply, State#state{health_check_socket = undefined}}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    % Clean up health check endpoint
    stop_health_check_endpoint(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

get_default_health_checks() ->
    #{
        port_manager => #health_check{
            name = port_manager,
            check_fun = fun check_port_manager_health/0,
            last_result = undefined,
            last_check_time = undefined
        },
        application => #health_check{
            name = application,
            check_fun = fun check_application_health/0,
            last_result = undefined,
            last_check_time = undefined
        }
    }.

start_health_check_endpoint(State) ->
    % Get health check port from configuration
    HealthCheckPort = get_health_check_port(),
    
    case gen_tcp:listen(HealthCheckPort, [binary, {packet, http_bin}, {active, true}, {reuseaddr, true}]) of
        {ok, Socket} ->
            error_logger:info_msg("Health check endpoint listening on port ~p~n", [HealthCheckPort]),
            State#state{
                health_check_port = HealthCheckPort,
                health_check_socket = Socket
            };
        {error, Reason} ->
            error_logger:error_msg("Failed to start health check endpoint on port ~p: ~p~n", 
                                 [HealthCheckPort, Reason]),
            State
    end.

stop_health_check_endpoint(State) ->
    case State#state.health_check_socket of
        undefined ->
            State;
        Socket ->
            gen_tcp:close(Socket),
            error_logger:info_msg("Health check endpoint stopped~n"),
            State#state{
                health_check_port = undefined,
                health_check_socket = undefined
            }
    end.

get_health_check_port() ->
    % Check for container-specific health check port configuration
    case os:getenv("HEALTH_CHECK_PORT") of
        false ->
            case application:get_env(erlvectordb, health_check_port) of
                {ok, Port} when is_integer(Port) -> Port;
                _ -> ?DEFAULT_HEALTH_CHECK_PORT
            end;
        PortStr ->
            try
                list_to_integer(PortStr)
            catch
                error:badarg -> ?DEFAULT_HEALTH_CHECK_PORT
            end
    end.

handle_health_check_request(Socket, Data, State) ->
    case Data of
        {http_request, 'GET', {abs_path, "/health"}, _Version} ->
            {OverallStatus, _CheckResults} = run_all_health_checks(State),
            send_health_response(Socket, OverallStatus);
        {http_request, 'GET', {abs_path, "/health/detailed"}, _Version} ->
            {OverallStatus, CheckResults} = run_all_health_checks(State),
            send_detailed_health_response(Socket, OverallStatus, CheckResults);
        {http_request, 'GET', {abs_path, "/ready"}, _Version} ->
            % Readiness check - simpler than health check
            send_readiness_response(Socket);
        _ ->
            send_error_response(Socket, 404, "Not Found")
    end.

run_all_health_checks(State) ->
    CheckResults = maps:fold(fun(Name, HealthCheck, Acc) ->
        Result = run_single_health_check(HealthCheck),
        maps:put(Name, Result, Acc)
    end, #{}, State#state.checks),
    
    % Determine overall status
    OverallStatus = determine_overall_status(CheckResults),
    
    {OverallStatus, CheckResults}.

run_single_health_check(HealthCheck) ->
    CheckFun = HealthCheck#health_check.check_fun,
    StartTime = erlang:system_time(microsecond),
    
    {Status, Details} = try
        CheckFun()
    catch
        Class:Reason:Stacktrace ->
            {unhealthy, #{
                error => Class,
                reason => Reason,
                stacktrace => Stacktrace
            }}
    end,
    
    EndTime = erlang:system_time(microsecond),
    Duration = EndTime - StartTime,
    
    #{
        status => Status,
        details => Details,
        duration_us => Duration,
        timestamp => erlang:system_time(second)
    }.

determine_overall_status(CheckResults) ->
    Statuses = [maps:get(status, Result) || Result <- maps:values(CheckResults)],
    
    case lists:member(unhealthy, Statuses) of
        true -> unhealthy;
        false ->
            case lists:member(degraded, Statuses) of
                true -> degraded;
                false -> healthy
            end
    end.

send_health_response(Socket, OverallStatus) ->
    StatusCode = case OverallStatus of
        healthy -> 200;
        degraded -> 200;  % Still considered OK for basic health check
        unhealthy -> 503
    end,
    
    Body = jsx:encode(#{
        status => OverallStatus,
        timestamp => erlang:system_time(second)
    }),
    
    send_http_response(Socket, StatusCode, Body).

send_detailed_health_response(Socket, OverallStatus, CheckResults) ->
    StatusCode = case OverallStatus of
        healthy -> 200;
        degraded -> 200;
        unhealthy -> 503
    end,
    
    Body = jsx:encode(#{
        overall_status => OverallStatus,
        checks => CheckResults,
        timestamp => erlang:system_time(second)
    }),
    
    send_http_response(Socket, StatusCode, Body).

send_readiness_response(Socket) ->
    % Simple readiness check - just verify the application is running
    case application:get_application(erlvectordb) of
        {ok, _} ->
            Body = jsx:encode(#{
                status => ready,
                timestamp => erlang:system_time(second)
            }),
            send_http_response(Socket, 200, Body);
        undefined ->
            Body = jsx:encode(#{
                status => not_ready,
                reason => application_not_running,
                timestamp => erlang:system_time(second)
            }),
            send_http_response(Socket, 503, Body)
    end.

send_http_response(Socket, StatusCode, Body) ->
    StatusText = http_status_text(StatusCode),
    Response = io_lib:format(
        "HTTP/1.1 ~w ~s\r\n"
        "Content-Type: application/json\r\n"
        "Content-Length: ~w\r\n"
        "Connection: close\r\n"
        "\r\n"
        "~s",
        [StatusCode, StatusText, byte_size(Body), Body]
    ),
    gen_tcp:send(Socket, Response),
    gen_tcp:close(Socket).

send_error_response(Socket, StatusCode, StatusText) ->
    Body = jsx:encode(#{
        error => StatusText,
        timestamp => erlang:system_time(second)
    }),
    send_http_response(Socket, StatusCode, Body).

http_status_text(200) -> "OK";
http_status_text(404) -> "Not Found";
http_status_text(503) -> "Service Unavailable";
http_status_text(_) -> "Unknown".

%%====================================================================
%% Default Health Check Functions
%%====================================================================

check_port_manager_health() ->
    try
        case port_manager:get_port_status() of
            {ok, PortStatus} ->
                % Check if all required services have bound ports
                BoundServices = [maps:get(service, Status) || Status <- PortStatus,
                               maps:get(status, Status) =:= bound],
                
                RequiredServices = get_required_services(),
                MissingServices = RequiredServices -- BoundServices,
                
                case MissingServices of
                    [] ->
                        {healthy, #{
                            bound_services => BoundServices,
                            total_ports => length(PortStatus)
                        }};
                    _ ->
                        {degraded, #{
                            bound_services => BoundServices,
                            missing_services => MissingServices,
                            total_ports => length(PortStatus)
                        }}
                end;
            {error, Reason} ->
                {unhealthy, #{error => port_manager_error, reason => Reason}}
        end
    catch
        _:Error ->
            {unhealthy, #{error => port_manager_exception, reason => Error}}
    end.

check_application_health() ->
    try
        % Check if the main application processes are running
        ProcessChecks = [
            {port_manager, whereis(port_manager)},
            {port_registry, whereis(port_registry)}
        ],
        
        {RunningProcesses, DeadProcesses} = lists:partition(fun({_Name, Pid}) ->
            is_pid(Pid) andalso is_process_alive(Pid)
        end, ProcessChecks),
        
        case DeadProcesses of
            [] ->
                {healthy, #{
                    running_processes => [Name || {Name, _} <- RunningProcesses],
                    node => node(),
                    uptime_seconds => get_application_uptime()
                }};
            _ ->
                {unhealthy, #{
                    running_processes => [Name || {Name, _} <- RunningProcesses],
                    dead_processes => [Name || {Name, _} <- DeadProcesses],
                    node => node()
                }}
        end
    catch
        _:Error ->
            {unhealthy, #{error => application_check_exception, reason => Error}}
    end.

get_required_services() ->
    % Get list of required services from configuration
    Config = port_config:load_config(),
    RequiredServices = maps:fold(fun(Service, ServiceConfig, Acc) ->
        Required = maps:get(required, ServiceConfig, true),
        case Required of
            true -> [Service | Acc];
            false -> Acc
        end
    end, [], Config),
    RequiredServices.

get_application_uptime() ->
    % Get application uptime in seconds
    case application:get_env(erlvectordb, start_time) of
        {ok, StartTime} ->
            erlang:system_time(second) - StartTime;
        undefined ->
            0
    end.
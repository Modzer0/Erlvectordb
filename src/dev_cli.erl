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

-module(dev_cli).

%% API for development mode command-line operations
-export([
    main/1,
    force_restart/0,
    force_restart/1,
    kill_existing/0,
    kill_existing/1,
    status/0,
    help/0
]).

%% Types
-type service_name() :: mcp_server | oauth_server | rest_api_server.

%%====================================================================
%% API
%%====================================================================

main(Args) ->
    try
        case Args of
            [] ->
                help(),
                halt(0);
            ["help"] ->
                help(),
                halt(0);
            ["--help"] ->
                help(),
                halt(0);
            ["-h"] ->
                help(),
                halt(0);
            ["status"] ->
                status(),
                halt(0);
            ["force-restart"] ->
                force_restart(),
                halt(0);
            ["force-restart" | Services] ->
                ServiceAtoms = parse_services(Services),
                force_restart(ServiceAtoms),
                halt(0);
            ["kill-existing"] ->
                kill_existing(),
                halt(0);
            ["kill-existing" | Services] ->
                ServiceAtoms = parse_services(Services),
                kill_existing(ServiceAtoms),
                halt(0);
            _ ->
                io:format("Unknown command: ~p~n", [Args]),
                help(),
                halt(1)
        end
    catch
        error:Reason ->
            io:format("Error: ~p~n", [Reason]),
            halt(1);
        throw:Reason ->
            io:format("Error: ~p~n", [Reason]),
            halt(1);
        exit:Reason ->
            io:format("Exit: ~p~n", [Reason]),
            halt(1)
    end.

force_restart() ->
    force_restart([mcp_server, oauth_server, rest_api_server]).

force_restart(Services) when is_list(Services) ->
    case port_config:is_development_mode() of
        false ->
            io:format("Error: Force restart is only available in development mode~n"),
            io:format("Set ERLVECTORDB_DEV_MODE=true or application environment development_mode=true~n"),
            halt(1);
        true ->
            io:format("Development Mode: Force restarting services: ~p~n", [Services]),
            
            % Ensure application is started
            case ensure_application_started() of
                ok ->
                    case port_manager:force_restart_all() of
                        {ok, {restarted, PortInfo}} ->
                            io:format("✓ Force restart successful!~n"),
                            print_port_info(PortInfo),
                            ok;
                        {error, not_in_development_mode} ->
                            io:format("Error: Port manager not in development mode~n"),
                            halt(1);
                        {error, {restart_failed, Reason}} ->
                            io:format("Error: Force restart failed: ~p~n", [Reason]),
                            halt(1);
                        {error, Reason} ->
                            io:format("Error: ~p~n", [Reason]),
                            halt(1)
                    end;
                {error, Reason} ->
                    io:format("Error starting application: ~p~n", [Reason]),
                    halt(1)
            end
    end.

kill_existing() ->
    kill_existing([mcp_server, oauth_server, rest_api_server]).

kill_existing(Services) when is_list(Services) ->
    case port_config:is_development_mode() of
        false ->
            io:format("Error: Kill existing is only available in development mode~n"),
            io:format("Set ERLVECTORDB_DEV_MODE=true or application environment development_mode=true~n"),
            halt(1);
        true ->
            io:format("Development Mode: Killing existing instances for services: ~p~n", [Services]),
            
            % Ensure application is started
            case ensure_application_started() of
                ok ->
                    case port_manager:kill_existing_instances(Services) of
                        {ok, KillResults} ->
                            io:format("✓ Kill existing completed!~n"),
                            print_kill_results(KillResults),
                            ok;
                        {error, not_in_development_mode} ->
                            io:format("Error: Port manager not in development mode~n"),
                            halt(1);
                        {error, Reason} ->
                            io:format("Error: ~p~n", [Reason]),
                            halt(1)
                    end;
                {error, Reason} ->
                    io:format("Error starting application: ~p~n", [Reason]),
                    halt(1)
            end
    end.

status() ->
    io:format("ErlVectorDB Development CLI Status~n"),
    io:format("================================~n"),
    
    % Check development mode
    DevMode = port_config:is_development_mode(),
    io:format("Development Mode: ~s~n", [format_boolean(DevMode)]),
    
    if DevMode ->
        DevConfig = port_config:get_development_config(),
        BasePorts = maps:get(base_ports, DevConfig, #{}),
        Timeout = maps:get(timeout_ms, DevConfig, undefined),
        
        io:format("Development Timeout: ~p ms~n", [Timeout]),
        io:format("Base Ports:~n"),
        maps:fold(fun(Service, Port, _) ->
            io:format("  ~s: ~p~n", [format_service_name(Service), Port])
        end, ok, BasePorts);
       true ->
        io:format("(Development mode configuration not available)~n")
    end,
    
    io:format("~n"),
    
    % Check application status
    case application:get_application(port_manager) of
        {ok, App} ->
            io:format("Application: ~p (running)~n", [App]);
        undefined ->
            io:format("Application: not running~n")
    end,
    
    % Check port manager status
    case whereis(port_manager) of
        undefined ->
            io:format("Port Manager: not running~n");
        Pid when is_pid(Pid) ->
            io:format("Port Manager: running (pid: ~p)~n", [Pid]),
            
            % Get port status if available
            try
                case port_manager:get_port_status() of
                    {ok, StatusInfo} ->
                        io:format("~nPort Allocations:~n"),
                        case StatusInfo of
                            [] ->
                                io:format("  (no ports allocated)~n");
                            _ ->
                                lists:foreach(fun(Info) ->
                                    Service = maps:get(service, Info),
                                    Port = maps:get(port, Info),
                                    Status = maps:get(status, Info),
                                    io:format("  ~s: port ~p (~s)~n", 
                                             [format_service_name(Service), Port, Status])
                                end, StatusInfo)
                        end;
                    {error, Reason} ->
                        io:format("Port Status Error: ~p~n", [Reason])
                end
            catch
                _:Error ->
                    io:format("Port Status Error: ~p~n", [Error])
            end
    end.

help() ->
    io:format("ErlVectorDB Development CLI~n"),
    io:format("=========================~n"),
    io:format("~n"),
    io:format("Usage: erl -pa ebin -s dev_cli main -s init stop -- [COMMAND] [OPTIONS]~n"),
    io:format("~n"),
    io:format("Commands:~n"),
    io:format("  status                    Show development mode and port status~n"),
    io:format("  force-restart             Force restart all services (dev mode only)~n"),
    io:format("  force-restart [SERVICES]  Force restart specific services~n"),
    io:format("  kill-existing             Kill existing instances of all services~n"),
    io:format("  kill-existing [SERVICES]  Kill existing instances of specific services~n"),
    io:format("  help                      Show this help message~n"),
    io:format("~n"),
    io:format("Services:~n"),
    io:format("  mcp-server               MCP Server~n"),
    io:format("  oauth-server             OAuth Server~n"),
    io:format("  rest-api-server          REST API Server~n"),
    io:format("~n"),
    io:format("Environment Variables:~n"),
    io:format("  ERLVECTORDB_DEV_MODE=true                Enable development mode~n"),
    io:format("  ERLVECTORDB_DEV_TIMEOUT_MS=2000          Set development timeout~n"),
    io:format("  ERLVECTORDB_DEV_MCP_SERVER_BASE_PORT=9080    Set MCP base port~n"),
    io:format("  ERLVECTORDB_DEV_OAUTH_SERVER_BASE_PORT=9081  Set OAuth base port~n"),
    io:format("  ERLVECTORDB_DEV_REST_API_SERVER_BASE_PORT=9082  Set REST API base port~n"),
    io:format("~n"),
    io:format("Examples:~n"),
    io:format("  # Show status~n"),
    io:format("  erl -pa ebin -s dev_cli main -s init stop -- status~n"),
    io:format("~n"),
    io:format("  # Force restart all services~n"),
    io:format("  ERLVECTORDB_DEV_MODE=true erl -pa ebin -s dev_cli main -s init stop -- force-restart~n"),
    io:format("~n"),
    io:format("  # Force restart only MCP server~n"),
    io:format("  ERLVECTORDB_DEV_MODE=true erl -pa ebin -s dev_cli main -s init stop -- force-restart mcp-server~n"),
    io:format("~n").

%%====================================================================
%% Internal functions
%%====================================================================

parse_services(ServiceStrings) ->
    lists:map(fun parse_service/1, ServiceStrings).

parse_service("mcp-server") -> mcp_server;
parse_service("mcp_server") -> mcp_server;
parse_service("oauth-server") -> oauth_server;
parse_service("oauth_server") -> oauth_server;
parse_service("rest-api-server") -> rest_api_server;
parse_service("rest_api_server") -> rest_api_server;
parse_service(ServiceStr) ->
    throw({unknown_service, ServiceStr}).

format_service_name(mcp_server) -> "MCP Server";
format_service_name(oauth_server) -> "OAuth Server";
format_service_name(rest_api_server) -> "REST API Server";
format_service_name(Service) -> atom_to_list(Service).

format_boolean(true) -> "enabled";
format_boolean(false) -> "disabled".

print_port_info(PortInfo) ->
    io:format("Port Allocations:~n"),
    lists:foreach(fun(Info) ->
        Service = maps:get(service, Info),
        Port = maps:get(port, Info),
        Status = maps:get(status, Info),
        io:format("  ~s: port ~p (~s)~n", 
                 [format_service_name(Service), Port, Status])
    end, PortInfo).

print_kill_results(KillResults) ->
    io:format("Kill Results:~n"),
    lists:foreach(fun({Service, Result}) ->
        ServiceName = format_service_name(Service),
        case Result of
            no_allocation ->
                io:format("  ~s: no allocation found~n", [ServiceName]);
            {port, Port, KillResult} ->
                io:format("  ~s: port ~p - ~s~n", 
                         [ServiceName, Port, format_kill_result(KillResult)])
        end
    end, KillResults).

format_kill_result({ok, port_was_free}) -> "port was already free";
format_kill_result({warning, port_in_use_cannot_kill}) -> "port in use, cannot kill automatically";
format_kill_result({error, Reason}) -> io_lib:format("error: ~p", [Reason]);
format_kill_result(Other) -> io_lib:format("~p", [Other]).

ensure_application_started() ->
    case application:ensure_all_started(erlvectordb) of
        {ok, _Started} -> ok;
        {error, Reason} -> {error, Reason}
    end.
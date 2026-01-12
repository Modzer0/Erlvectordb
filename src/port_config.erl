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

-module(port_config).

%% API
-export([
    load_config/0,
    get_service_config/1,
    validate_config/1,
    get_port_ranges/0
]).

%% Types
-type config_source() :: application_env | config_file | environment_vars.
-type service_config() :: #{
    preferred_port => integer(),
    port_range => {integer(), integer()},
    bind_interface => string(),
    required => boolean()
}.
-type service_name() :: mcp_server | oauth_server | rest_api_server.

%% Records
-record(service_config, {
    name :: service_name(),
    preferred_port :: integer(),
    port_range :: {integer(), integer()},
    bind_interface :: string(),
    required :: boolean(),
    startup_order :: integer(),
    health_check_path :: string() | undefined
}).

%%====================================================================
%% API
%%====================================================================

load_config() ->
    % Load configuration from multiple sources in priority order:
    % 1. Environment variables (highest priority)
    % 2. Application environment
    % 3. Default configuration (lowest priority)
    
    DefaultConfig = get_default_config(),
    AppEnvConfig = load_from_application_env(),
    EnvVarConfig = load_from_environment_vars(),
    
    % Merge configurations with proper precedence (later configs override earlier ones)
    merge_configs([DefaultConfig, AppEnvConfig, EnvVarConfig]).

get_service_config(Service) when is_atom(Service) ->
    Config = load_config(),
    maps:get(Service, Config, get_default_service_config(Service)).

validate_config(Config) when is_map(Config) ->
    try
        maps:fold(fun(Service, ServiceConfig, Acc) ->
            case validate_service_config(Service, ServiceConfig) of
                ok ->
                    Acc;
                {error, Reason} ->
                    throw({error, {invalid_service_config, Service, Reason}})
            end
        end, ok, Config),
        ok
    catch
        throw:Error ->
            Error;
        error:Reason ->
            {error, {validation_error, Reason}}
    end;
validate_config(Config) ->
    {error, {invalid_config_type, Config}}.

get_port_ranges() ->
    Config = load_config(),
    maps:fold(fun(Service, ServiceConfig, Acc) ->
        Range = maps:get(port_range, ServiceConfig, get_default_port_range(Service)),
        maps:put(Service, Range, Acc)
    end, #{}, Config).

%%====================================================================
%% Internal functions
%%====================================================================

get_default_config() ->
    #{
        mcp_server => get_default_service_config(mcp_server),
        oauth_server => get_default_service_config(oauth_server),
        rest_api_server => get_default_service_config(rest_api_server)
    }.

get_default_service_config(mcp_server) ->
    #{
        preferred_port => 8080,
        port_range => {8080, 8090},
        bind_interface => "127.0.0.1",
        required => true
    };
get_default_service_config(oauth_server) ->
    #{
        preferred_port => 8081,
        port_range => {8081, 8091},
        bind_interface => "127.0.0.1",
        required => true
    };
get_default_service_config(rest_api_server) ->
    #{
        preferred_port => 8082,
        port_range => {8082, 8092},
        bind_interface => "127.0.0.1",
        required => true
    }.

get_default_port_range(mcp_server) -> {8080, 8090};
get_default_port_range(oauth_server) -> {8081, 8091};
get_default_port_range(rest_api_server) -> {8082, 8092}.

load_from_application_env() ->
    case application:get_env(erlvectordb, port_config) of
        {ok, Config} when is_map(Config) ->
            Config;
        {ok, Config} when is_list(Config) ->
            % Convert proplist to map
            try
                maps:from_list(Config)
            catch
                error:_ ->
                    error_logger:warning_msg("Invalid port_config format in application env, using defaults~n"),
                    #{}
            end;
        undefined ->
            % Try individual service configurations
            load_individual_service_configs();
        Other ->
            error_logger:warning_msg("Invalid port_config in application env: ~p, using defaults~n", [Other]),
            #{}
    end.

load_individual_service_configs() ->
    Services = [mcp_server, oauth_server, rest_api_server],
    lists:foldl(fun(Service, Acc) ->
        case load_service_from_app_env(Service) of
            {ok, ServiceConfig} ->
                maps:put(Service, ServiceConfig, Acc);
            error ->
                Acc
        end
    end, #{}, Services).

load_service_from_app_env(Service) ->
    ServiceAtom = list_to_atom(atom_to_list(Service) ++ "_config"),
    case application:get_env(erlvectordb, ServiceAtom) of
        {ok, Config} when is_map(Config) ->
            {ok, Config};
        {ok, Config} when is_list(Config) ->
            try
                {ok, maps:from_list(Config)}
            catch
                error:_ ->
                    error
            end;
        _ ->
            error
    end.

load_from_environment_vars() ->
    Services = [mcp_server, oauth_server, rest_api_server],
    lists:foldl(fun(Service, Acc) ->
        case load_service_from_env_vars(Service) of
            {ok, ServiceConfig} ->
                maps:put(Service, ServiceConfig, Acc);
            error ->
                Acc
        end
    end, #{}, Services).

load_service_from_env_vars(Service) ->
    ServiceStr = string:to_upper(atom_to_list(Service)),
    
    % Try to load port configuration from environment variables
    PortVar = ServiceStr ++ "_PORT",
    RangeStartVar = ServiceStr ++ "_PORT_RANGE_START", 
    RangeEndVar = ServiceStr ++ "_PORT_RANGE_END",
    InterfaceVar = ServiceStr ++ "_BIND_INTERFACE",
    
    Config = #{},
    
    % Load preferred port
    Config1 = case os:getenv(PortVar) of
        false -> Config;
        PortStr ->
            try
                Port = list_to_integer(PortStr),
                maps:put(preferred_port, Port, Config)
            catch
                error:badarg ->
                    error_logger:warning_msg("Invalid port in ~s: ~s~n", [PortVar, PortStr]),
                    Config
            end
    end,
    
    % Load port range
    Config2 = case {os:getenv(RangeStartVar), os:getenv(RangeEndVar)} of
        {false, false} -> Config1;
        {StartStr, EndStr} when StartStr =/= false, EndStr =/= false ->
            try
                Start = list_to_integer(StartStr),
                End = list_to_integer(EndStr),
                maps:put(port_range, {Start, End}, Config1)
            catch
                error:badarg ->
                    error_logger:warning_msg("Invalid port range in ~s/~s: ~s/~s~n", 
                                           [RangeStartVar, RangeEndVar, StartStr, EndStr]),
                    Config1
            end;
        _ ->
            Config1
    end,
    
    % Load bind interface
    Config3 = case os:getenv(InterfaceVar) of
        false -> Config2;
        Interface -> maps:put(bind_interface, Interface, Config2)
    end,
    
    case maps:size(Config3) of
        0 -> error;
        _ -> {ok, Config3}
    end.

merge_configs(Configs) ->
    lists:foldl(fun(Config, Acc) ->
        maps:fold(fun(Service, ServiceConfig, AccInner) ->
            ExistingConfig = maps:get(Service, AccInner, #{}),
            MergedServiceConfig = maps:merge(ExistingConfig, ServiceConfig),
            maps:put(Service, MergedServiceConfig, AccInner)
        end, Acc, Config)
    end, #{}, Configs).

validate_service_config(Service, Config) when is_map(Config) ->
    try
        % Validate preferred port
        case maps:get(preferred_port, Config, undefined) of
            undefined -> ok;
            Port when is_integer(Port), Port >= 1024, Port =< 65535 -> ok;
            Port -> throw({invalid_preferred_port, Port})
        end,
        
        % Validate port range
        case maps:get(port_range, Config, undefined) of
            undefined -> ok;
            {Start, End} when is_integer(Start), is_integer(End),
                             Start >= 1024, End =< 65535, Start =< End -> ok;
            Range -> throw({invalid_port_range, Range})
        end,
        
        % Validate bind interface
        case maps:get(bind_interface, Config, undefined) of
            undefined -> ok;
            Interface when is_list(Interface) -> ok;
            Interface -> throw({invalid_bind_interface, Interface})
        end,
        
        % Validate required flag
        case maps:get(required, Config, undefined) of
            undefined -> ok;
            Required when is_boolean(Required) -> ok;
            Required -> throw({invalid_required_flag, Required})
        end,
        
        ok
    catch
        throw:Reason ->
            {error, Reason}
    end;
validate_service_config(_Service, Config) ->
    {error, {invalid_config_type, Config}}.
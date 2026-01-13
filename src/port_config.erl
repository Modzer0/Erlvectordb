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
    get_port_ranges/0,
    get_service_config_with_defaults/1,
    reload_config/0,
    set_service_config/2,
    is_development_mode/0,
    get_development_config/0,
    get_development_base_ports/0,
    get_development_timeout/0,
    is_container_mode/0,
    get_container_config/0,
    get_container_bind_interface/0,
    should_bind_all_interfaces/0,
    should_log_port_mappings/0,
    get_container_shutdown_timeout/0
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
    % 2. Configuration files
    % 3. Application environment
    % 4. Default configuration (lowest priority)
    
    DefaultConfig = get_default_config(),
    AppEnvConfig = load_from_application_env(),
    FileConfig = load_from_config_file(),
    EnvVarConfig = load_from_environment_vars(),
    
    % Merge configurations with proper precedence (later configs override earlier ones)
    Config = merge_configs([DefaultConfig, AppEnvConfig, FileConfig, EnvVarConfig]),
    
    % Validate the final configuration
    case validate_config(Config) of
        ok ->
            Config;
        {error, Reason} ->
            error_logger:error_msg("Invalid port configuration: ~p, using defaults~n", [Reason]),
            get_default_config()
    end.

get_service_config(Service) when is_atom(Service) ->
    Config = load_config(),
    maps:get(Service, Config, get_default_service_config(Service)).

validate_config(Config) when is_map(Config) ->
    try
        ValidationResults = maps:fold(fun(Service, ServiceConfig, Acc) ->
            case validate_service_config(Service, ServiceConfig) of
                ok ->
                    Acc;
                {error, Reason} ->
                    [{Service, Reason} | Acc]
            end
        end, [], Config),
        
        case ValidationResults of
            [] ->
                ok;
            Errors ->
                {error, {validation_errors, Errors}}
        end
    catch
        error:Reason ->
            {error, {validation_exception, Reason}}
    end;
validate_config(Config) ->
    {error, {invalid_config_type, Config}}.

get_port_ranges() ->
    Config = load_config(),
    maps:fold(fun(Service, ServiceConfig, Acc) ->
        Range = maps:get(port_range, ServiceConfig, get_default_port_range(Service)),
        maps:put(Service, Range, Acc)
    end, #{}, Config).

get_service_config_with_defaults(Service) when is_atom(Service) ->
    % Get service configuration with full defaults applied
    Config = load_config(),
    ServiceConfig = maps:get(Service, Config, #{}),
    DefaultConfig = get_default_service_config(Service),
    maps:merge(DefaultConfig, ServiceConfig).

reload_config() ->
    % Force reload of configuration from all sources
    % This is useful when configuration files or environment variables change
    load_config().

set_service_config(Service, ServiceConfig) when is_atom(Service), is_map(ServiceConfig) ->
    % Set configuration for a specific service in application environment
    % This is primarily for testing and dynamic configuration
    case validate_service_config(Service, ServiceConfig) of
        ok ->
            CurrentConfig = case application:get_env(erlvectordb, port_config) of
                {ok, Config} when is_map(Config) -> Config;
                _ -> #{}
            end,
            NewConfig = maps:put(Service, ServiceConfig, CurrentConfig),
            application:set_env(erlvectordb, port_config, NewConfig),
            ok;
        {error, Reason} ->
            {error, {invalid_service_config, Service, Reason}}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

get_default_config() ->
    #{
        mcp_server => get_default_service_config(mcp_server),
        oauth_server => get_default_service_config(oauth_server),
        rest_api_server => get_default_service_config(rest_api_server)
    }.

get_default_service_config(Service) ->
    % Check if we're in development mode and adjust defaults accordingly
    case is_development_mode() of
        true ->
            get_development_service_config(Service);
        false ->
            get_production_service_config(Service)
    end.

get_production_service_config(mcp_server) ->
    BaseConfig = #{
        preferred_port => 8080,
        port_range => {8080, 8090},
        bind_interface => get_default_bind_interface(),
        required => true,
        startup_order => 1,
        health_check_path => "/health",
        service_name => "MCP Server",
        description => "Model Context Protocol Server"
    },
    apply_container_overrides(mcp_server, BaseConfig);
get_production_service_config(oauth_server) ->
    BaseConfig = #{
        preferred_port => 8081,
        port_range => {8081, 8091},
        bind_interface => get_default_bind_interface(),
        required => true,
        startup_order => 2,
        health_check_path => "/oauth/health",
        service_name => "OAuth Server",
        description => "OAuth Authentication Server"
    },
    apply_container_overrides(oauth_server, BaseConfig);
get_production_service_config(rest_api_server) ->
    BaseConfig = #{
        preferred_port => 8082,
        port_range => {8082, 8092},
        bind_interface => get_default_bind_interface(),
        required => true,
        startup_order => 3,
        health_check_path => "/api/health",
        service_name => "REST API Server",
        description => "RESTful API Server"
    },
    apply_container_overrides(rest_api_server, BaseConfig).

get_development_service_config(Service) ->
    % Get base development configuration
    DevConfig = get_development_config(),
    BasePorts = maps:get(base_ports, DevConfig, #{}),
    RangeSize = maps:get(port_range_size, DevConfig, 20),
    
    % Get the base port for this service
    BasePort = maps:get(Service, BasePorts, get_default_dev_port(Service)),
    
    % Create development-specific configuration
    ProductionConfig = get_production_service_config(Service),
    
    % Override with development settings
    maps:merge(ProductionConfig, #{
        preferred_port => BasePort,
        port_range => {BasePort, BasePort + RangeSize - 1},
        bind_interface => "127.0.0.1",  % Keep localhost for development
        development_mode => true,
        timeout_ms => maps:get(timeout_ms, DevConfig, 2000)
    }).

get_default_dev_port(mcp_server) -> 9080;
get_default_dev_port(oauth_server) -> 9081;
get_default_dev_port(rest_api_server) -> 9082.

get_default_port_range(mcp_server) -> {8080, 8090};
get_default_port_range(oauth_server) -> {8081, 8091};
get_default_port_range(rest_api_server) -> {8082, 8092}.

load_from_application_env() ->
    % First try the new structured port_config format
    case application:get_env(erlvectordb, port_config) of
        {ok, Config} when is_map(Config) ->
            Config;
        {ok, Config} when is_list(Config) ->
            % Convert proplist to map
            try
                maps:from_list(Config)
            catch
                error:_ ->
                    error_logger:warning_msg("Invalid port_config format in application env, trying individual configs~n"),
                    load_individual_service_configs()
            end;
        undefined ->
            % Try individual service configurations and legacy format
            LegacyConfig = load_legacy_port_config(),
            IndividualConfig = load_individual_service_configs(),
            merge_configs([LegacyConfig, IndividualConfig]);
        Other ->
            error_logger:warning_msg("Invalid port_config in application env: ~p, trying individual configs~n", [Other]),
            load_individual_service_configs()
    end.

load_legacy_port_config() ->
    % Support legacy port configuration format
    Config = #{},
    
    % Load legacy port settings
    Config1 = case application:get_env(erlvectordb, mcp_port) of
        {ok, Port1} when is_integer(Port1) ->
            ServiceConfig1 = #{preferred_port => Port1},
            maps:put(mcp_server, ServiceConfig1, Config);
        _ -> Config
    end,
    
    Config2 = case application:get_env(erlvectordb, oauth_port) of
        {ok, Port2} when is_integer(Port2) ->
            ServiceConfig2 = #{preferred_port => Port2},
            maps:put(oauth_server, ServiceConfig2, Config1);
        _ -> Config1
    end,
    
    case application:get_env(erlvectordb, rest_api_port) of
        {ok, Port3} when is_integer(Port3) ->
            ServiceConfig3 = #{preferred_port => Port3},
            maps:put(rest_api_server, ServiceConfig3, Config2);
        _ -> Config2
    end.

load_from_config_file() ->
    % Try to load from various config file locations
    ConfigPaths = [
        "config/port_config.config",
        "priv/port_config.config", 
        "port_config.config"
    ],
    
    load_from_config_paths(ConfigPaths).

load_from_config_paths([]) ->
    #{};
load_from_config_paths([Path | Rest]) ->
    case file:consult(Path) of
        {ok, Terms} ->
            try
                parse_config_file_terms(Terms)
            catch
                error:Reason ->
                    error_logger:warning_msg("Error parsing config file ~s: ~p, trying next~n", [Path, Reason]),
                    load_from_config_paths(Rest)
            end;
        {error, enoent} ->
            % File doesn't exist, try next
            load_from_config_paths(Rest);
        {error, Reason} ->
            error_logger:warning_msg("Error reading config file ~s: ~p, trying next~n", [Path, Reason]),
            load_from_config_paths(Rest)
    end.

parse_config_file_terms(Terms) ->
    % Parse Erlang terms from config file
    % Expected format: [{port_config, ConfigMap}] or [ConfigMap]
    case Terms of
        [{port_config, Config}] when is_map(Config) ->
            Config;
        [{port_config, Config}] when is_list(Config) ->
            maps:from_list(Config);
        [Config] when is_map(Config) ->
            Config;
        [Config] when is_list(Config) ->
            maps:from_list(Config);
        Config when is_list(Config) ->
            % Try to parse as proplist
            maps:from_list(Config);
        _ ->
            error({invalid_config_file_format, Terms})
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
    RequiredVar = ServiceStr ++ "_REQUIRED",
    
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
    
    % Load required flag
    Config4 = case os:getenv(RequiredVar) of
        false -> Config3;
        "true" -> maps:put(required, true, Config3);
        "false" -> maps:put(required, false, Config3);
        RequiredStr ->
            error_logger:warning_msg("Invalid required flag in ~s: ~s (expected 'true' or 'false')~n", 
                                   [RequiredVar, RequiredStr]),
            Config3
    end,
    
    % Check for container-specific environment variables
    Config5 = load_container_env_vars(Service, Config4),
    
    case maps:size(Config5) of
        0 -> error;
        _ -> {ok, Config5}
    end.

load_container_env_vars(Service, Config) ->
    % Enhanced container environment variable loading
    Config1 = load_generic_container_vars(Config),
    Config2 = load_service_specific_container_vars(Service, Config1),
    Config2.

load_generic_container_vars(Config) ->
    % Check for Docker/container-specific PORT environment variable
    Config1 = case os:getenv("PORT") of
        false -> Config;
        PortStr ->
            try
                Port = list_to_integer(PortStr),
                % Only use PORT env var if no service-specific port is set
                case maps:is_key(preferred_port, Config) of
                    false -> maps:put(preferred_port, Port, Config);
                    true -> Config
                end
            catch
                error:badarg ->
                    error_logger:warning_msg("Invalid PORT environment variable: ~s~n", [PortStr]),
                    Config
            end
    end,
    
    % Check for container bind interface
    Config2 = case should_bind_all_interfaces() of
        true -> maps:put(bind_interface, "0.0.0.0", Config1);
        false -> 
            case maps:is_key(bind_interface, Config1) of
                false -> maps:put(bind_interface, "127.0.0.1", Config1);
                true -> Config1
            end
    end,
    
    % Add container-specific settings if in container mode
    case is_container_mode() of
        true ->
            ContainerSettings = #{
                container_mode => true,
                health_check_enabled => true,
                graceful_shutdown_timeout => get_container_shutdown_timeout()
            },
            maps:merge(Config2, ContainerSettings);
        false ->
            Config2
    end.

load_service_specific_container_vars(Service, Config) ->
    % Load service-specific container environment variables
    ServiceStr = string:to_upper(atom_to_list(Service)),
    
    % Service-specific PORT variable (overrides generic PORT)
    ServicePortVar = ServiceStr ++ "_PORT",
    Config1 = case os:getenv(ServicePortVar) of
        false -> Config;
        PortStr ->
            case parse_port_from_env(PortStr, ServicePortVar) of
                {ok, Port} -> maps:put(preferred_port, Port, Config);
                error -> Config
            end
    end,
    
    % Service-specific bind interface
    ServiceBindVar = ServiceStr ++ "_BIND_INTERFACE",
    Config2 = case os:getenv(ServiceBindVar) of
        false -> Config1;
        Interface -> maps:put(bind_interface, Interface, Config1)
    end,
    
    % Service-specific health check path
    ServiceHealthVar = ServiceStr ++ "_HEALTH_PATH",
    Config3 = case os:getenv(ServiceHealthVar) of
        false -> Config2;
        HealthPath -> maps:put(health_check_path, HealthPath, Config2)
    end,
    
    Config3.

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
        Validations = [
            fun() -> validate_preferred_port(maps:get(preferred_port, Config, undefined)) end,
            fun() -> validate_port_range(maps:get(port_range, Config, undefined)) end,
            fun() -> validate_bind_interface(maps:get(bind_interface, Config, undefined)) end,
            fun() -> validate_required_flag(maps:get(required, Config, undefined)) end,
            fun() -> validate_service_specific(Service, Config) end
        ],
        
        lists:foreach(fun(ValidationFun) ->
            case ValidationFun() of
                ok -> ok;
                {error, Reason} -> throw(Reason)
            end
        end, Validations),
        
        ok
    catch
        throw:Reason ->
            {error, Reason}
    end;
validate_service_config(_Service, Config) ->
    {error, {invalid_config_type, Config}}.

%%====================================================================
%% Development Mode Functions
%%====================================================================

is_development_mode() ->
    % Check multiple sources for development mode indication
    case application:get_env(erlvectordb, development_mode) of
        {ok, true} -> true;
        {ok, "true"} -> true;
        _ ->
            case os:getenv("ERLVECTORDB_DEV_MODE") of
                "true" -> true;
                "1" -> true;
                _ ->
                    case os:getenv("NODE_ENV") of
                        "development" -> true;
                        "dev" -> true;
                        _ -> false
                    end
            end
    end.

get_development_config() ->
    % Get development-specific configuration
    BaseConfig = #{
        enabled => is_development_mode(),
        base_ports => get_development_base_ports(),
        timeout_ms => get_development_timeout(),
        auto_increment => true,
        port_range_size => 20  % Larger range for development
    },
    
    % Override with any explicit development configuration
    case application:get_env(erlvectordb, development_config) of
        {ok, DevConfig} when is_map(DevConfig) ->
            maps:merge(BaseConfig, DevConfig);
        {ok, DevConfig} when is_list(DevConfig) ->
            try
                ExplicitConfig = maps:from_list(DevConfig),
                maps:merge(BaseConfig, ExplicitConfig)
            catch
                error:_ -> BaseConfig
            end;
        _ ->
            BaseConfig
    end.

get_development_base_ports() ->
    % Get base ports for development mode
    DefaultBasePorts = #{
        mcp_server => 9080,
        oauth_server => 9081,
        rest_api_server => 9082
    },
    
    % Check for environment variable overrides
    EnvBasePorts = load_dev_base_ports_from_env(),
    
    % Check for application environment overrides
    AppBasePorts = case application:get_env(erlvectordb, dev_base_ports) of
        {ok, Ports} when is_map(Ports) -> Ports;
        {ok, Ports} when is_list(Ports) ->
            try maps:from_list(Ports)
            catch error:_ -> #{}
            end;
        _ -> #{}
    end,
    
    % Merge with precedence: env vars > app env > defaults
    maps:merge(maps:merge(DefaultBasePorts, AppBasePorts), EnvBasePorts).

get_development_timeout() ->
    % Get timeout for development mode (shorter than production)
    DefaultTimeout = 2000,  % 2 seconds instead of default 5 seconds
    
    case application:get_env(erlvectordb, dev_timeout_ms) of
        {ok, Timeout} when is_integer(Timeout), Timeout > 0 ->
            Timeout;
        _ ->
            case os:getenv("ERLVECTORDB_DEV_TIMEOUT_MS") of
                false -> DefaultTimeout;
                TimeoutStr ->
                    try
                        Timeout = list_to_integer(TimeoutStr),
                        if Timeout > 0 -> Timeout;
                           true -> DefaultTimeout
                        end
                    catch
                        error:badarg -> DefaultTimeout
                    end
            end
    end.

load_dev_base_ports_from_env() ->
    % Load development base ports from environment variables
    Services = [mcp_server, oauth_server, rest_api_server],
    lists:foldl(fun(Service, Acc) ->
        ServiceStr = string:to_upper(atom_to_list(Service)),
        EnvVar = "ERLVECTORDB_DEV_" ++ ServiceStr ++ "_BASE_PORT",
        
        case os:getenv(EnvVar) of
            false -> Acc;
            PortStr ->
                try
                    Port = list_to_integer(PortStr),
                    if Port >= 1024 andalso Port =< 65535 ->
                        maps:put(Service, Port, Acc);
                       true ->
                        error_logger:warning_msg("Invalid development base port in ~s: ~s (must be 1024-65535)~n", 
                                               [EnvVar, PortStr]),
                        Acc
                    end
                catch
                    error:badarg ->
                        error_logger:warning_msg("Invalid development base port in ~s: ~s (not an integer)~n", 
                                               [EnvVar, PortStr]),
                        Acc
                end
        end
    end, #{}, Services).

validate_preferred_port(undefined) -> ok;
validate_preferred_port(Port) when is_integer(Port) ->
    if
        Port < 1024 -> {error, {preferred_port_too_low, Port, 1024}};
        Port > 65535 -> {error, {preferred_port_too_high, Port, 65535}};
        true -> ok
    end;
validate_preferred_port(Port) ->
    {error, {invalid_preferred_port_type, Port}}.

validate_port_range(undefined) -> ok;
validate_port_range({Start, End}) when is_integer(Start), is_integer(End) ->
    if
        Start < 1024 -> {error, {port_range_start_too_low, Start, 1024}};
        End > 65535 -> {error, {port_range_end_too_high, End, 65535}};
        Start > End -> {error, {invalid_port_range_order, Start, End}};
        End - Start < 0 -> {error, {port_range_too_small, Start, End}};
        true -> ok
    end;
validate_port_range(Range) ->
    {error, {invalid_port_range_format, Range}}.

validate_bind_interface(undefined) -> ok;
validate_bind_interface(Interface) when is_list(Interface) ->
    % Basic validation - could be enhanced with IP address parsing
    case length(Interface) of
        0 -> {error, empty_bind_interface};
        _ -> ok
    end;
validate_bind_interface(Interface) ->
    {error, {invalid_bind_interface_type, Interface}}.

validate_required_flag(undefined) -> ok;
validate_required_flag(Required) when is_boolean(Required) -> ok;
validate_required_flag(Required) ->
    {error, {invalid_required_flag_type, Required}}.

validate_service_specific(Service, Config) ->
    % Service-specific validation rules
    case Service of
        mcp_server -> validate_mcp_config(Config);
        oauth_server -> validate_oauth_config(Config);
        rest_api_server -> validate_rest_api_config(Config);
        _ -> {error, {unknown_service, Service}}
    end.

validate_mcp_config(_Config) ->
    % MCP-specific validation
    ok.

validate_oauth_config(_Config) ->
    % OAuth-specific validation
    ok.

validate_rest_api_config(_Config) ->
    % REST API-specific validation
    ok.

%%====================================================================
%% Container Mode Functions
%%====================================================================

is_container_mode() ->
    % Check multiple indicators for container deployment
    case application:get_env(erlvectordb, container_mode) of
        {ok, true} -> true;
        {ok, "true"} -> true;
        _ ->
            % Check environment variables that indicate container deployment
            ContainerIndicators = [
                {"CONTAINER", "true"},
                {"DOCKER", "true"},
                {"KUBERNETES_SERVICE_HOST", fun(V) -> V =/= false end},
                {"HOSTNAME", fun(V) -> V =/= false andalso is_container_hostname(V) end}
            ],
            
            lists:any(fun({EnvVar, Expected}) ->
                case os:getenv(EnvVar) of
                    false -> false;
                    Value when is_function(Expected) -> Expected(Value);
                    Value -> Value =:= Expected
                end
            end, ContainerIndicators)
    end.

is_container_hostname(Hostname) ->
    % Container hostnames are often random hex strings or follow specific patterns
    % This is a heuristic check
    case length(Hostname) of
        12 -> is_hex_string(Hostname);  % Docker short container ID
        64 -> is_hex_string(Hostname);  % Docker full container ID
        _ -> 
            % Check for Kubernetes pod naming patterns
            string:str(Hostname, "-") > 0 andalso 
            (string:str(Hostname, "pod") > 0 orelse 
             string:str(Hostname, "deployment") > 0)
    end.

is_hex_string(Str) ->
    lists:all(fun(C) -> 
        (C >= $0 andalso C =< $9) orelse 
        (C >= $a andalso C =< $f) orelse 
        (C >= $A andalso C =< $F)
    end, Str).

get_container_config() ->
    % Get container-specific configuration
    BaseConfig = #{
        enabled => is_container_mode(),
        bind_interface => get_container_bind_interface(),
        health_check_enabled => true,
        graceful_shutdown_timeout => get_container_shutdown_timeout(),
        port_mapping_logging => should_log_port_mappings()
    },
    
    % Override with any explicit container configuration
    case application:get_env(erlvectordb, container_config) of
        {ok, ContainerConfig} when is_map(ContainerConfig) ->
            maps:merge(BaseConfig, ContainerConfig);
        {ok, ContainerConfig} when is_list(ContainerConfig) ->
            try
                ExplicitConfig = maps:from_list(ContainerConfig),
                maps:merge(BaseConfig, ExplicitConfig)
            catch
                error:_ -> BaseConfig
            end;
        _ ->
            BaseConfig
    end.

get_container_bind_interface() ->
    % Determine the appropriate bind interface for container deployment
    case should_bind_all_interfaces() of
        true -> "0.0.0.0";
        false -> "127.0.0.1"
    end.

should_bind_all_interfaces() ->
    % Check if we should bind to all interfaces (0.0.0.0)
    case os:getenv("BIND_ALL_INTERFACES") of
        "true" -> true;
        "1" -> true;
        "false" -> false;
        "0" -> false;
        false ->
            % Auto-detect based on container mode
            case is_container_mode() of
                true ->
                    % In container mode, default to binding all interfaces
                    % unless explicitly configured otherwise
                    case application:get_env(erlvectordb, bind_all_interfaces) of
                        {ok, false} -> false;
                        {ok, "false"} -> false;
                        _ -> true
                    end;
                false ->
                    false
            end;
        _ ->
            % Invalid value, default to false for security
            error_logger:warning_msg("Invalid BIND_ALL_INTERFACES value, defaulting to false~n"),
            false
    end.

get_default_bind_interface() ->
    % Get the default bind interface based on deployment mode
    case is_container_mode() of
        true -> get_container_bind_interface();
        false -> "127.0.0.1"
    end.

apply_container_overrides(Service, BaseConfig) ->
    case is_container_mode() of
        false -> BaseConfig;
        true ->
            ContainerConfig = get_container_config(),
            
            % Apply container-specific overrides
            Config1 = maps:put(bind_interface, 
                              maps:get(bind_interface, ContainerConfig, "0.0.0.0"), 
                              BaseConfig),
            
            % Check for service-specific PORT environment variable
            Config2 = apply_container_port_override(Service, Config1),
            
            % Add container-specific metadata
            maps:merge(Config2, #{
                container_mode => true,
                health_check_enabled => maps:get(health_check_enabled, ContainerConfig, true),
                graceful_shutdown_timeout => maps:get(graceful_shutdown_timeout, ContainerConfig, 30000)
            })
    end.

apply_container_port_override(Service, Config) ->
    % Check for service-specific PORT environment variables
    ServiceStr = string:to_upper(atom_to_list(Service)),
    ServicePortVar = ServiceStr ++ "_PORT",
    
    % First check service-specific PORT variable
    case os:getenv(ServicePortVar) of
        false ->
            % Then check generic PORT variable (typically used for single-service containers)
            case os:getenv("PORT") of
                false -> Config;
                PortStr ->
                    case parse_port_from_env(PortStr, "PORT") of
                        {ok, Port} -> maps:put(preferred_port, Port, Config);
                        error -> Config
                    end
            end;
        PortStr ->
            case parse_port_from_env(PortStr, ServicePortVar) of
                {ok, Port} -> maps:put(preferred_port, Port, Config);
                error -> Config
            end
    end.

parse_port_from_env(PortStr, EnvVarName) ->
    try
        Port = list_to_integer(PortStr),
        if Port >= 1024 andalso Port =< 65535 ->
            {ok, Port};
           true ->
            error_logger:warning_msg("Invalid port in ~s: ~s (must be 1024-65535)~n", 
                                   [EnvVarName, PortStr]),
            error
        end
    catch
        error:badarg ->
            error_logger:warning_msg("Invalid port format in ~s: ~s (not an integer)~n", 
                                   [EnvVarName, PortStr]),
            error
    end.

get_container_shutdown_timeout() ->
    % Get graceful shutdown timeout for containers
    DefaultTimeout = 30000,  % 30 seconds
    
    case os:getenv("GRACEFUL_SHUTDOWN_TIMEOUT") of
        false ->
            case application:get_env(erlvectordb, graceful_shutdown_timeout) of
                {ok, Timeout} when is_integer(Timeout), Timeout > 0 -> Timeout;
                _ -> DefaultTimeout
            end;
        TimeoutStr ->
            try
                Timeout = list_to_integer(TimeoutStr),
                if Timeout > 0 -> Timeout * 1000;  % Convert seconds to milliseconds
                   true -> DefaultTimeout
                end
            catch
                error:badarg -> DefaultTimeout
            end
    end.

should_log_port_mappings() ->
    % Determine if we should log port mappings (useful for container orchestration)
    case os:getenv("LOG_PORT_MAPPINGS") of
        "true" -> true;
        "1" -> true;
        "false" -> false;
        "0" -> false;
        false ->
            % Default to true in container mode for debugging
            is_container_mode();
        _ ->
            is_container_mode()
    end.
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

-module(erlvectordb_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    
    % Port manager must start first to allocate ports for other services
    PortManagerSpec = #{id => port_manager,
                       start => {port_manager, start_link, []},
                       restart => permanent,
                       shutdown => 5000,
                       type => worker,
                       modules => [port_manager]},
    
    % Startup coordinator manages the sequenced startup of network services
    StartupCoordinatorSpec = #{id => startup_coordinator,
                              start => {startup_coordinator, start_link, []},
                              restart => permanent,
                              shutdown => 5000,
                              type => worker,
                              modules => [startup_coordinator]},
    
    % Services that don't need port management can start normally
    % Network services (mcp_server, oauth_http_handler, rest_api_server) 
    % will be started by the startup coordinator after port allocation
    ChildSpecs = [
        PortManagerSpec,
        StartupCoordinatorSpec,
        
        % Health check server for container deployments
        #{id => health_check_server,
          start => {health_check_server, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [health_check_server]},
          
        % Signal handler for graceful shutdown
        #{id => signal_handler,
          start => {signal_handler, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [signal_handler]},
        
        #{id => cluster_manager,
          start => {cluster_manager, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [cluster_manager]},
          
        #{id => oauth_server,
          start => {oauth_server, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [oauth_server]},
        
        #{id => vector_store_sup,
          start => {vector_store_sup, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => supervisor,
          modules => [vector_store_sup]},
          
        #{id => vector_index_manager,
          start => {vector_index_manager, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [vector_index_manager]}
    ],
    
    {ok, {SupFlags, ChildSpecs}}.

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
    
    ChildSpecs = [
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
          
        #{id => oauth_http_handler,
          start => {oauth_http_handler, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [oauth_http_handler]},
          
        #{id => rest_api_server,
          start => {rest_api_server, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [rest_api_server]},
        
        #{id => vector_store_sup,
          start => {vector_store_sup, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => supervisor,
          modules => [vector_store_sup]},
        
        #{id => mcp_server,
          start => {mcp_server, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [mcp_server]},
          
        #{id => vector_index_manager,
          start => {vector_index_manager, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [vector_index_manager]}
    ],
    
    {ok, {SupFlags, ChildSpecs}}.

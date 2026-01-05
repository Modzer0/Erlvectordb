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

-module(vector_store_sup).
-behaviour(supervisor).

-export([start_link/0, start_store/1, stop_store/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_store(StoreName) ->
    ChildSpec = #{id => StoreName,
                  start => {vector_store, start_link, [StoreName]},
                  restart => permanent,
                  shutdown => 5000,
                  type => worker,
                  modules => [vector_store]},
    supervisor:start_child(?SERVER, ChildSpec).

stop_store(StoreName) ->
    supervisor:terminate_child(?SERVER, StoreName),
    supervisor:delete_child(?SERVER, StoreName).

init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 10,
                 period => 60},
    {ok, {SupFlags, []}}.

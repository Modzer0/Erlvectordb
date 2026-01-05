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

-module(vector_index_manager).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([create_index/2, build_index/1, get_index_stats/1]).

-record(state, {
    indexes = #{} :: map()
}).

-record(index_info, {
    name :: atom(),
    type :: hnsw | ivf | flat,
    parameters :: map(),
    built = false :: boolean(),
    stats = #{} :: map()
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_index(IndexName, Parameters) ->
    gen_server:call(?MODULE, {create_index, IndexName, Parameters}).

build_index(IndexName) ->
    gen_server:call(?MODULE, {build_index, IndexName}).

get_index_stats(IndexName) ->
    gen_server:call(?MODULE, {get_index_stats, IndexName}).

init([]) ->
    {ok, #state{}}.

handle_call({create_index, IndexName, Parameters}, _From, State) ->
    IndexType = maps:get(type, Parameters, flat),
    IndexInfo = #index_info{
        name = IndexName,
        type = IndexType,
        parameters = Parameters
    },
    NewIndexes = maps:put(IndexName, IndexInfo, State#state.indexes),
    {reply, ok, State#state{indexes = NewIndexes}};

handle_call({build_index, IndexName}, _From, State) ->
    case maps:get(IndexName, State#state.indexes, undefined) of
        undefined ->
            {reply, {error, index_not_found}, State};
        IndexInfo ->
            % Simulate index building process
            spawn(fun() -> build_index_async(IndexName, IndexInfo) end),
            {reply, {ok, building}, State}
    end;

handle_call({get_index_stats, IndexName}, _From, State) ->
    case maps:get(IndexName, State#state.indexes, undefined) of
        undefined ->
            {reply, {error, index_not_found}, State};
        IndexInfo ->
            {reply, {ok, IndexInfo#index_info.stats}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({index_built, IndexName, Stats}, State) ->
    case maps:get(IndexName, State#state.indexes, undefined) of
        undefined ->
            {noreply, State};
        IndexInfo ->
            UpdatedInfo = IndexInfo#index_info{built = true, stats = Stats},
            NewIndexes = maps:put(IndexName, UpdatedInfo, State#state.indexes),
            {noreply, State#state{indexes = NewIndexes}}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
build_index_async(IndexName, IndexInfo) ->
    % Simulate building process
    timer:sleep(1000),
    Stats = #{
        build_time => 1000,
        memory_usage => 1024 * 1024,
        index_size => 512 * 1024
    },
    gen_server:cast(?MODULE, {index_built, IndexName, Stats}).

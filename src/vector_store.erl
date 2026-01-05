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

-module(vector_store).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([insert/3, search/3, delete/2, get_stats/1]).

-record(state, {
    name :: atom(),
    vectors = #{} :: map(),
    metadata = #{} :: map(),
    index :: term(),
    dimension :: integer() | undefined
}).

-record(vector_entry, {
    id :: binary(),
    vector :: [float()],
    metadata :: map()
}).

%% API
start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name], []).

insert(StoreName, VectorId, VectorData) ->
    gen_server:call(StoreName, {insert, VectorId, VectorData}).

search(StoreName, QueryVector, K) ->
    gen_server:call(StoreName, {search, QueryVector, K}).

delete(StoreName, VectorId) ->
    gen_server:call(StoreName, {delete, VectorId}).

get_stats(StoreName) ->
    gen_server:call(StoreName, get_stats).

%% Callbacks
init([Name]) ->
    process_flag(trap_exit, true),
    {ok, #state{name = Name}}.

handle_call({insert, VectorId, #{vector := Vector, metadata := Metadata}}, _From, State) ->
    case validate_vector(Vector, State#state.dimension) of
        {ok, Dimension} ->
            Entry = #vector_entry{
                id = VectorId,
                vector = Vector,
                metadata = Metadata
            },
            NewVectors = maps:put(VectorId, Entry, State#state.vectors),
            NewState = State#state{
                vectors = NewVectors,
                dimension = Dimension
            },
            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({search, QueryVector, K}, _From, State) ->
    case validate_vector(QueryVector, State#state.dimension) of
        {ok, _} ->
            Results = perform_search(QueryVector, K, State#state.vectors),
            {reply, {ok, Results}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({delete, VectorId}, _From, State) ->
    NewVectors = maps:remove(VectorId, State#state.vectors),
    NewState = State#state{vectors = NewVectors},
    {reply, ok, NewState};

handle_call(get_stats, _From, State) ->
    Stats = #{
        name => State#state.name,
        count => maps:size(State#state.vectors),
        dimension => State#state.dimension
    },
    {reply, {ok, Stats}, State};

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

%% Internal functions
validate_vector(Vector, undefined) when is_list(Vector) ->
    case lists:all(fun is_number/1, Vector) of
        true -> {ok, length(Vector)};
        false -> {error, invalid_vector_format}
    end;
validate_vector(Vector, Dimension) when is_list(Vector) ->
    case {lists:all(fun is_number/1, Vector), length(Vector)} of
        {true, Dimension} -> {ok, Dimension};
        {true, _} -> {error, dimension_mismatch};
        {false, _} -> {error, invalid_vector_format}
    end;
validate_vector(_, _) ->
    {error, invalid_vector_format}.

perform_search(QueryVector, K, Vectors) ->
    Distances = maps:fold(fun(Id, Entry, Acc) ->
        Distance = cosine_distance(QueryVector, Entry#vector_entry.vector),
        [{Distance, Id, Entry} | Acc]
    end, [], Vectors),
    
    Sorted = lists:sort(Distances),
    TopK = lists:sublist(Sorted, K),
    
    [{Id, Entry#vector_entry.metadata, Distance} || {Distance, Id, Entry} <- TopK].

cosine_distance(V1, V2) ->
    DotProduct = dot_product(V1, V2),
    Norm1 = vector_norm(V1),
    Norm2 = vector_norm(V2),
    case {Norm1, Norm2} of
        {0.0, _} -> 1.0;
        {_, 0.0} -> 1.0;
        _ -> 1.0 - (DotProduct / (Norm1 * Norm2))
    end.

dot_product(V1, V2) ->
    lists:sum([X * Y || {X, Y} <- lists:zip(V1, V2)]).

vector_norm(Vector) ->
    math:sqrt(lists:sum([X * X || X <- Vector])).

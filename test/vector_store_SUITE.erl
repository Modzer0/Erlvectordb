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

-module(vector_store_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() ->
    [
        test_create_store,
        test_insert_vector,
        test_search_vectors,
        test_delete_vector,
        test_dimension_validation
    ].

init_per_suite(Config) ->
    application:ensure_all_started(erlvectordb),
    Config.

end_per_suite(_Config) ->
    application:stop(erlvectordb),
    ok.

init_per_testcase(TestCase, Config) ->
    % Create unique store name for each test case to ensure isolation
    StoreName = list_to_atom("test_store_" ++ atom_to_list(TestCase)),
    {ok, _Pid} = vector_store_sup:start_store(StoreName),
    [{store_name, StoreName} | Config].

end_per_testcase(_TestCase, Config) ->
    StoreName = ?config(store_name, Config),
    vector_store_sup:stop_store(StoreName),
    % Clean up any leftover files for this specific store
    DataDir = application:get_env(erlvectordb, persistence_dir, "test_data"),
    DetsFile = filename:join(DataDir, atom_to_list(StoreName) ++ ".dets"),
    file:delete(DetsFile),
    ok.

test_create_store(Config) ->
    StoreName = ?config(store_name, Config),
    {ok, Stats} = vector_store:get_stats(StoreName),
    #{count := 0} = Stats.

test_insert_vector(Config) ->
    StoreName = ?config(store_name, Config),
    Vector = [1.0, 2.0, 3.0],
    Metadata = #{type => test, category => example},
    VectorData = #{vector => Vector, metadata => Metadata},
    
    ok = vector_store:insert(StoreName, <<"test1">>, VectorData),
    
    {ok, Stats} = vector_store:get_stats(StoreName),
    #{count := 1, dimension := 3} = Stats.

test_search_vectors(Config) ->
    StoreName = ?config(store_name, Config),
    
    % Insert test vectors
    Vectors = [
        {<<"v1">>, [1.0, 0.0, 0.0]},
        {<<"v2">>, [0.0, 1.0, 0.0]},
        {<<"v3">>, [0.0, 0.0, 1.0]}
    ],
    
    lists:foreach(fun({Id, Vector}) ->
        VectorData = #{vector => Vector, metadata => #{id => Id}},
        ok = vector_store:insert(StoreName, Id, VectorData)
    end, Vectors),
    
    % Search for similar vector
    QueryVector = [1.0, 0.1, 0.1],
    {ok, Results} = vector_store:search(StoreName, QueryVector, 2),
    
    % Should return 2 results, with v1 being most similar
    2 = length(Results),
    [{<<"v1">>, _, _} | _] = Results.

test_delete_vector(Config) ->
    StoreName = ?config(store_name, Config),
    Vector = [1.0, 2.0, 3.0],
    VectorData = #{vector => Vector, metadata => #{}},
    
    ok = vector_store:insert(StoreName, <<"test1">>, VectorData),
    {ok, #{count := 1}} = vector_store:get_stats(StoreName),
    
    ok = vector_store:delete(StoreName, <<"test1">>),
    {ok, #{count := 0}} = vector_store:get_stats(StoreName).

test_dimension_validation(Config) ->
    StoreName = ?config(store_name, Config),
    
    % Insert first vector with 3 dimensions
    Vector1 = [1.0, 2.0, 3.0],
    VectorData1 = #{vector => Vector1, metadata => #{}},
    ok = vector_store:insert(StoreName, <<"test1">>, VectorData1),
    
    % Try to insert vector with different dimensions
    Vector2 = [1.0, 2.0],
    VectorData2 = #{vector => Vector2, metadata => #{}},
    {error, dimension_mismatch} = vector_store:insert(StoreName, <<"test2">>, VectorData2).

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

-module(persistence_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-record(backup_info, {
    store_name :: atom(),
    timestamp :: integer(),
    file_path :: string(),
    vector_count :: integer(),
    dimension :: integer() | undefined
}).

all() ->
    [
        test_persistence_save_load,
        test_store_restart_persistence,
        test_backup_restore,
        test_export_import,
        test_sync_operations
    ].

init_per_suite(Config) ->
    application:set_env(erlvectordb, persistence_enabled, true),
    application:set_env(erlvectordb, persistence_dir, "test_data"),
    application:set_env(erlvectordb, backup_dir, "test_backups"),
    application:ensure_all_started(erlvectordb),
    Config.

end_per_suite(_Config) ->
    application:stop(erlvectordb),
    % Clean up test directories
    os:cmd("rm -rf test_data test_backups"),
    ok.

init_per_testcase(TestCase, Config) ->
    % Create unique store name for each test case to ensure isolation
    StoreName = list_to_atom("test_persistence_store_" ++ atom_to_list(TestCase)),
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

test_persistence_save_load(Config) ->
    StoreName = ?config(store_name, Config),
    
    % Insert test vectors
    Vectors = [
        {<<"v1">>, [1.0, 2.0, 3.0], #{type => test1}},
        {<<"v2">>, [4.0, 5.0, 6.0], #{type => test2}},
        {<<"v3">>, [7.0, 8.0, 9.0], #{type => test3}}
    ],
    
    lists:foreach(fun({Id, Vector, Metadata}) ->
        VectorData = #{vector => Vector, metadata => Metadata},
        ok = vector_store:insert(StoreName, Id, VectorData)
    end, Vectors),
    
    % Force sync to persistence
    ok = vector_store:sync(StoreName),
    
    % Verify data is persisted by loading directly from persistence
    {ok, LoadedVectors} = vector_persistence:load_vectors(StoreName),
    3 = maps:size(LoadedVectors),
    
    % Verify specific vector
    #{vector := [1.0, 2.0, 3.0], metadata := #{type := test1}} = maps:get(<<"v1">>, LoadedVectors).

test_store_restart_persistence(Config) ->
    StoreName = ?config(store_name, Config),
    
    % Insert test data
    VectorData = #{vector => [1.0, 2.0, 3.0], metadata => #{persistent => true}},
    ok = vector_store:insert(StoreName, <<"persistent_vector">>, VectorData),
    ok = vector_store:sync(StoreName),
    
    % Stop and restart the store
    vector_store_sup:stop_store(StoreName),
    {ok, _NewPid} = vector_store_sup:start_store(StoreName),
    
    % Verify data is still there
    {ok, Results} = vector_store:search(StoreName, [1.0, 2.0, 3.0], 1),
    1 = length(Results),
    [{<<"persistent_vector">>, #{persistent := true}, _Distance}] = Results.

test_backup_restore(Config) ->
    StoreName = ?config(store_name, Config),
    
    % Insert test data
    TestVectors = [
        {<<"backup1">>, [1.0, 0.0, 0.0], #{category => backup_test}},
        {<<"backup2">>, [0.0, 1.0, 0.0], #{category => backup_test}}
    ],
    
    lists:foreach(fun({Id, Vector, Metadata}) ->
        VectorData = #{vector => Vector, metadata => Metadata},
        ok = vector_store:insert(StoreName, Id, VectorData)
    end, TestVectors),
    
    % Create backup
    {ok, BackupInfo} = vector_backup:backup_store(StoreName, "test_backup"),
    BackupPath = BackupInfo#backup_info.file_path,
    
    % Restore to new store
    NewStoreName = restored_test_store,
    {ok, RestoreResult} = vector_backup:restore_store(BackupPath, NewStoreName),
    
    2 = maps:get(vectors_restored, RestoreResult),
    [] = maps:get(errors, RestoreResult),
    
    % Verify restored data
    {ok, Results} = vector_store:search(NewStoreName, [1.0, 0.0, 0.0], 1),
    1 = length(Results),
    [{<<"backup1">>, #{category := backup_test}, _}] = Results,
    
    % Clean up
    vector_store_sup:stop_store(NewStoreName).

test_export_import(Config) ->
    StoreName = ?config(store_name, Config),
    
    % Insert test data
    VectorData = #{vector => [2.0, 3.0, 4.0], metadata => #{export_test => true}},
    ok = vector_store:insert(StoreName, <<"export_vector">>, VectorData),
    
    % Export to JSON
    ExportPath = "test_export.json",
    {ok, _ExportResult} = vector_backup:export_store(StoreName, ExportPath),
    
    % Import to new store
    ImportStoreName = imported_test_store,
    {ok, ImportResult} = vector_backup:import_store(ExportPath, ImportStoreName),
    
    1 = maps:get(vectors_imported, ImportResult),
    
    % Verify imported data
    {ok, Results} = vector_store:search(ImportStoreName, [2.0, 3.0, 4.0], 1),
    1 = length(Results),
    [Result] = Results,
    {<<"export_vector">>, Metadata, Distance} = Result,
    true = maps:is_key(export_test, Metadata) orelse maps:is_key(<<"export_test">>, Metadata),
    % Distance should be very close to 0 (allowing for floating point precision)
    true = abs(Distance) < 0.0001,
    
    % Clean up
    vector_store_sup:stop_store(ImportStoreName),
    file:delete(ExportPath).

test_sync_operations(Config) ->
    StoreName = ?config(store_name, Config),
    
    % Insert data without immediate sync
    VectorData = #{vector => [5.0, 6.0, 7.0], metadata => #{sync_test => true}},
    ok = vector_store:insert(StoreName, <<"sync_vector">>, VectorData),
    
    % Manually sync
    ok = vector_store:sync(StoreName),
    
    % Verify sync worked by checking persistence directly
    {ok, LoadedVectors} = vector_persistence:load_vectors(StoreName),
    true = maps:is_key(<<"sync_vector">>, LoadedVectors),
    
    #{vector := [5.0, 6.0, 7.0], metadata := #{sync_test := true}} = 
        maps:get(<<"sync_vector">>, LoadedVectors).
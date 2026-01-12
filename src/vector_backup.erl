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

-module(vector_backup).

-export([
    backup_store/2,
    restore_store/2,
    list_backups/0,
    delete_backup/1,
    export_store/2,
    import_store/2
]).

-record(backup_info, {
    store_name :: atom(),
    timestamp :: integer(),
    file_path :: string(),
    vector_count :: integer(),
    dimension :: integer() | undefined
}).

-record(store_info, {
    name :: atom(),
    dimension :: integer() | undefined,
    count :: integer(),
    created :: integer(),
    last_modified :: integer()
}).

%% Create a backup of a vector store
backup_store(StoreName, BackupName) ->
    try
        % Get current store data
        {ok, Vectors} = vector_persistence:load_vectors(StoreName),
        {ok, StoreInfo} = vector_persistence:get_store_info(StoreName),
        
        % Create backup directory
        BackupDir = application:get_env(erlvectordb, backup_dir, "backups"),
        ok = filelib:ensure_dir(filename:join(BackupDir, "dummy")),
        
        Timestamp = erlang:system_time(millisecond),
        BackupFileName = lists:flatten(io_lib:format("~s_~s_~p.backup", 
                                                    [StoreName, BackupName, Timestamp])),
        BackupPath = filename:join(BackupDir, BackupFileName),
        
        % Create backup data structure
        BackupData = #{
            store_name => StoreName,
            backup_name => BackupName,
            timestamp => Timestamp,
            store_info => StoreInfo,
            vectors => Vectors
        },
        
        % Write backup file
        case file:write_file(BackupPath, term_to_binary(BackupData)) of
            ok ->
                BackupInfo = #backup_info{
                    store_name = StoreName,
                    timestamp = Timestamp,
                    file_path = BackupPath,
                    vector_count = maps:size(Vectors),
                    dimension = case StoreInfo of
                                   {ok, Info} -> Info#store_info.dimension;
                                   _ -> undefined
                               end
                },
                {ok, BackupInfo};
            {error, Reason} ->
                {error, {backup_write_failed, Reason}}
        end
    catch
        CatchError:CatchReason:_Stacktrace ->
            {error, {backup_failed, CatchError, CatchReason}}
    end.

%% Restore a vector store from backup
restore_store(BackupPath, NewStoreName) ->
    try
        case file:read_file(BackupPath) of
            {ok, BackupBinary} ->
                BackupData = binary_to_term(BackupBinary),
                
                #{
                    vectors := Vectors,
                    store_info := StoreInfo
                } = BackupData,
                
                % Create new store
                case vector_store_sup:start_store(NewStoreName) of
                    {ok, _Pid} ->
                        % Restore vectors
                        RestoreResults = maps:fold(fun(VectorId, VectorData, Acc) ->
                            Vector = maps:get(vector, VectorData),
                            Metadata = maps:get(metadata, VectorData),
                            
                            case vector_store:insert(NewStoreName, VectorId, 
                                                   #{vector => Vector, metadata => Metadata}) of
                                ok -> Acc;
                                {error, Reason} -> [{error, VectorId, Reason} | Acc]
                            end
                        end, [], Vectors),
                        
                        % Force sync to persistence
                        vector_store:sync(NewStoreName),
                        
                        case RestoreResults of
                            [] ->
                                {ok, #{
                                    store_name => NewStoreName,
                                    vectors_restored => maps:size(Vectors),
                                    errors => []
                                }};
                            Errors ->
                                {ok, #{
                                    store_name => NewStoreName,
                                    vectors_restored => maps:size(Vectors) - length(Errors),
                                    errors => Errors
                                }}
                        end;
                    {error, Reason} ->
                        {error, {store_creation_failed, Reason}}
                end;
            {error, Reason} ->
                {error, {backup_read_failed, Reason}}
        end
    catch
        CatchError:CatchReason:_Stacktrace ->
            {error, {restore_failed, CatchError, CatchReason}}
    end.

%% List all available backups
list_backups() ->
    BackupDir = application:get_env(erlvectordb, backup_dir, "backups"),
    case file:list_dir(BackupDir) of
        {ok, Files} ->
            BackupFiles = [F || F <- Files, filename:extension(F) =:= ".backup"],
            BackupInfos = lists:filtermap(fun(File) ->
                FilePath = filename:join(BackupDir, File),
                case get_backup_info(FilePath) of
                    {ok, Info} -> {true, Info};
                    {error, _} -> false
                end
            end, BackupFiles),
            {ok, lists:sort(fun(A, B) -> 
                A#backup_info.timestamp >= B#backup_info.timestamp 
            end, BackupInfos)};
        {error, Reason} ->
            {error, {list_backups_failed, Reason}}
    end.

%% Delete a backup file
delete_backup(BackupPath) ->
    case file:delete(BackupPath) of
        ok -> ok;
        {error, Reason} -> {error, {delete_backup_failed, Reason}}
    end.

%% Export store to JSON format
export_store(StoreName, ExportPath) ->
    try
        {ok, Vectors} = vector_persistence:load_vectors(StoreName),
        {ok, StoreInfo} = vector_persistence:get_store_info(StoreName),
        
        ExportData = #{
            <<"store_name">> => atom_to_binary(StoreName, utf8),
            <<"export_timestamp">> => erlang:system_time(millisecond),
            <<"store_info">> => store_info_to_map(StoreInfo),
            <<"vectors">> => vectors_to_json_map(Vectors)
        },
        
        JsonData = jsx:encode(ExportData),
        case file:write_file(ExportPath, JsonData) of
            ok -> {ok, #{path => ExportPath, vector_count => maps:size(Vectors)}};
            {error, Reason} -> {error, {export_write_failed, Reason}}
        end
    catch
        CatchError:CatchReason:_Stacktrace ->
            {error, {export_failed, CatchError, CatchReason}}
    end.

%% Import store from JSON format
import_store(ImportPath, StoreName) ->
    try
        case file:read_file(ImportPath) of
            {ok, JsonData} ->
                ImportData = jsx:decode(JsonData, [return_maps]),
                
                Vectors = maps:get(<<"vectors">>, ImportData),
                
                % Create new store
                case vector_store_sup:start_store(StoreName) of
                    {ok, _Pid} ->
                        % Import vectors
                        ImportResults = maps:fold(fun(VectorId, VectorData, Acc) ->
                            Vector = maps:get(<<"vector">>, VectorData),
                            Metadata = maps:get(<<"metadata">>, VectorData, #{}),
                            
                            case vector_store:insert(StoreName, VectorId, 
                                                   #{vector => Vector, metadata => Metadata}) of
                                ok -> Acc;
                                {error, Reason} -> [{error, VectorId, Reason} | Acc]
                            end
                        end, [], Vectors),
                        
                        % Force sync to persistence
                        vector_store:sync(StoreName),
                        
                        case ImportResults of
                            [] ->
                                {ok, #{
                                    store_name => StoreName,
                                    vectors_imported => maps:size(Vectors),
                                    errors => []
                                }};
                            Errors ->
                                {ok, #{
                                    store_name => StoreName,
                                    vectors_imported => maps:size(Vectors) - length(Errors),
                                    errors => Errors
                                }}
                        end;
                    {error, Reason} ->
                        {error, {store_creation_failed, Reason}}
                end;
            {error, Reason} ->
                {error, {import_read_failed, Reason}}
        end
    catch
        CatchError:CatchReason:_Stacktrace ->
            {error, {import_failed, CatchError, CatchReason}}
    end.

%% Internal functions
get_backup_info(BackupPath) ->
    try
        case file:read_file(BackupPath) of
            {ok, BackupBinary} ->
                BackupData = binary_to_term(BackupBinary),
                
                #{
                    store_name := StoreName,
                    timestamp := Timestamp,
                    vectors := Vectors,
                    store_info := StoreInfo
                } = BackupData,
                
                Info = #backup_info{
                    store_name = StoreName,
                    timestamp = Timestamp,
                    file_path = BackupPath,
                    vector_count = maps:size(Vectors),
                    dimension = case StoreInfo of
                                   {ok, SI} -> SI#store_info.dimension;
                                   _ -> undefined
                               end
                },
                {ok, Info};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        _:_ ->
            {error, invalid_backup_file}
    end.

store_info_to_map({ok, StoreInfo}) ->
    #{
        <<"name">> => atom_to_binary(StoreInfo#store_info.name, utf8),
        <<"dimension">> => StoreInfo#store_info.dimension,
        <<"count">> => StoreInfo#store_info.count,
        <<"created">> => StoreInfo#store_info.created,
        <<"last_modified">> => StoreInfo#store_info.last_modified
    };
store_info_to_map(_) ->
    #{}.

vectors_to_json_map(Vectors) ->
    maps:fold(fun(Id, VectorData, Acc) ->
        JsonVectorData = #{
            <<"vector">> => maps:get(vector, VectorData),
            <<"metadata">> => maps:get(metadata, VectorData),
            <<"timestamp">> => maps:get(timestamp, VectorData, 0)
        },
        maps:put(Id, JsonVectorData, Acc)
    end, #{}, Vectors).
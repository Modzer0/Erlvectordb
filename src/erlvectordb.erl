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

-module(erlvectordb).

%% API exports
-export([
    start/0,
    stop/0,
    create_store/1,
    delete_store/1,
    insert/3,
    insert/4,
    search/3,
    search/4,
    delete/2,
    get_stats/1,
    list_stores/0,
    sync/1,
    backup_store/2,
    restore_store/2,
    export_store/2,
    import_store/2,
    list_backups/0
]).

%% Application control
start() ->
    application:ensure_all_started(erlvectordb).

stop() ->
    application:stop(erlvectordb).

%% Store management
create_store(StoreName) when is_atom(StoreName) ->
    vector_store_sup:start_store(StoreName).

delete_store(StoreName) when is_atom(StoreName) ->
    vector_store_sup:stop_store(StoreName).

list_stores() ->
    supervisor:which_children(vector_store_sup).

%% Vector operations
insert(StoreName, VectorId, Vector) ->
    insert(StoreName, VectorId, Vector, #{}).

insert(StoreName, VectorId, Vector, Metadata) when is_atom(StoreName) ->
    VectorData = #{vector => Vector, metadata => Metadata},
    vector_store:insert(StoreName, VectorId, VectorData).

search(StoreName, QueryVector, K) ->
    search(StoreName, QueryVector, K, #{}).

search(StoreName, QueryVector, K, _Options) when is_atom(StoreName) ->
    vector_store:search(StoreName, QueryVector, K).

delete(StoreName, VectorId) when is_atom(StoreName) ->
    vector_store:delete(StoreName, VectorId).

get_stats(StoreName) when is_atom(StoreName) ->
    vector_store:get_stats(StoreName).

%% Persistence operations
sync(StoreName) when is_atom(StoreName) ->
    vector_store:sync(StoreName).

%% Backup and restore operations
backup_store(StoreName, BackupName) when is_atom(StoreName) ->
    vector_backup:backup_store(StoreName, BackupName).

restore_store(BackupPath, NewStoreName) when is_atom(NewStoreName) ->
    vector_backup:restore_store(BackupPath, NewStoreName).

export_store(StoreName, ExportPath) when is_atom(StoreName) ->
    vector_backup:export_store(StoreName, ExportPath).

import_store(ImportPath, StoreName) when is_atom(StoreName) ->
    vector_backup:import_store(ImportPath, StoreName).

list_backups() ->
    vector_backup:list_backups().

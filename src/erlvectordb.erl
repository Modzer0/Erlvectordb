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
    create_distributed_store/2,
    delete_store/1,
    insert/3,
    insert/4,
    insert_compressed/4,
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
    list_backups/0,
    register_oauth_client/3,
    get_oauth_token/3,
    refresh_oauth_token/1,
    join_cluster/1,
    leave_cluster/0,
    get_cluster_status/0,
    compress_vector/2,
    benchmark_compression/2
]).

%% Application control
start() ->
    application:ensure_all_started(erlvectordb).

stop() ->
    application:stop(erlvectordb).

%% Store management
create_store(StoreName) when is_atom(StoreName) ->
    vector_store_sup:start_store(StoreName).

create_distributed_store(StoreName, Options) when is_atom(StoreName) ->
    case application:get_env(erlvectordb, cluster_enabled, false) of
        true ->
            cluster_manager:distribute_store(StoreName, Options);
        false ->
            vector_store_sup:start_store(StoreName)
    end.

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

insert_compressed(StoreName, VectorId, Vector, Metadata) when is_atom(StoreName) ->
    Algorithm = application:get_env(erlvectordb, compression_algorithm, quantization_8bit),
    case vector_compression:compress_vector(Vector, Algorithm) of
        {ok, CompressedVector} ->
            vector_persistence:save_compressed_vector(StoreName, VectorId, CompressedVector, Metadata);
        {error, Reason} ->
            {error, {compression_failed, Reason}}
    end.

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

%% OAuth operations
register_oauth_client(ClientId, ClientSecret, Options) ->
    oauth_client:register_client(ClientId, ClientSecret, Options).

get_oauth_token(ClientId, ClientSecret, Scopes) ->
    oauth_client:get_access_token(ClientId, ClientSecret, Scopes).

refresh_oauth_token(RefreshToken) ->
    oauth_client:refresh_access_token(RefreshToken).

%% Clustering operations
join_cluster(SeedNode) ->
    cluster_manager:join_cluster(SeedNode).

leave_cluster() ->
    cluster_manager:leave_cluster().

get_cluster_status() ->
    case application:get_env(erlvectordb, cluster_enabled, false) of
        true ->
            {ok, ClusterStats} = cluster_manager:get_cluster_stats(),
            {ok, ClusterNodes} = cluster_manager:get_cluster_nodes(),
            {ok, NodeStatus} = cluster_manager:get_node_status(),
            {ok, #{
                cluster_enabled => true,
                current_node => node(),
                node_status => NodeStatus,
                cluster_nodes => ClusterNodes,
                stats => ClusterStats
            }};
        false ->
            {ok, #{
                cluster_enabled => false,
                current_node => node()
            }}
    end.

%% Compression operations
compress_vector(Vector, Algorithm) ->
    vector_compression:compress_vector(Vector, Algorithm).

benchmark_compression(Vector, Algorithms) ->
    vector_compression:benchmark_compression(Vector, Algorithms).

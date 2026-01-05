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

-module(clustering_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() ->
    [
        test_cluster_manager_start,
        test_store_distribution,
        test_cluster_stats,
        test_node_selection
    ].

init_per_suite(Config) ->
    application:set_env(erlvectordb, cluster_enabled, true),
    application:set_env(erlvectordb, node_name, 'test_node@localhost'),
    application:ensure_all_started(erlvectordb),
    Config.

end_per_suite(_Config) ->
    application:stop(erlvectordb),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

test_cluster_manager_start(_Config) ->
    % Test that cluster manager starts successfully
    {ok, NodeStatus} = cluster_manager:get_node_status(),
    true = is_atom(NodeStatus),
    
    {ok, ClusterNodes} = cluster_manager:get_cluster_nodes(),
    true = is_list(ClusterNodes).

test_store_distribution(_Config) ->
    StoreName = test_distributed_store,
    Options = #{replication_factor => 1}, % Single node for testing
    
    % Test store distribution
    {ok, Nodes} = cluster_manager:distribute_store(StoreName, Options),
    true = is_list(Nodes),
    true = length(Nodes) >= 1,
    
    % Test store location lookup
    {ok, FoundNodes} = cluster_manager:get_store_location(StoreName),
    Nodes = FoundNodes.

test_cluster_stats(_Config) ->
    {ok, Stats} = cluster_manager:get_cluster_stats(),
    
    % Verify stats structure
    true = is_map(Stats),
    true = maps:is_key(cluster_nodes, Stats),
    true = maps:is_key(node_status, Stats),
    true = maps:is_key(distributed_stores, Stats),
    true = maps:is_key(replication_factor, Stats),
    
    % Verify values
    ClusterNodes = maps:get(cluster_nodes, Stats),
    true = is_integer(ClusterNodes),
    true = ClusterNodes >= 1.

test_node_selection(_Config) ->
    % Test that node selection works for different replication factors
    StoreName1 = test_store_rf1,
    {ok, Nodes1} = cluster_manager:distribute_store(StoreName1, #{replication_factor => 1}),
    1 = length(Nodes1),
    
    StoreName2 = test_store_rf2,
    {ok, Nodes2} = cluster_manager:distribute_store(StoreName2, #{replication_factor => 2}),
    % Should be 1 since we only have one node in test
    true = length(Nodes2) =< 2.
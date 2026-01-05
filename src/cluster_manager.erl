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

-module(cluster_manager).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([
    join_cluster/1,
    leave_cluster/0,
    get_cluster_nodes/0,
    get_node_status/0,
    distribute_store/2,
    get_store_location/1,
    sync_cluster_state/0,
    get_cluster_stats/0
]).

-record(state, {
    cluster_nodes = [] :: [node()],
    node_status = active :: active | leaving | joining,
    store_distribution = #{} :: map(),
    replication_factor = 2 :: integer(),
    heartbeat_interval = 5000 :: integer()
}).

-record(node_info, {
    node :: node(),
    status :: active | inactive | joining | leaving,
    load :: integer(),
    stores = [] :: [atom()],
    last_seen :: integer()
}).

%% API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

join_cluster(SeedNode) ->
    gen_server:call(?MODULE, {join_cluster, SeedNode}).

leave_cluster() ->
    gen_server:call(?MODULE, leave_cluster).

get_cluster_nodes() ->
    gen_server:call(?MODULE, get_cluster_nodes).

get_node_status() ->
    gen_server:call(?MODULE, get_node_status).

distribute_store(StoreName, Options) ->
    gen_server:call(?MODULE, {distribute_store, StoreName, Options}).

get_store_location(StoreName) ->
    gen_server:call(?MODULE, {get_store_location, StoreName}).

sync_cluster_state() ->
    gen_server:call(?MODULE, sync_cluster_state).

get_cluster_stats() ->
    gen_server:call(?MODULE, get_cluster_stats).

%% Callbacks
init([]) ->
    process_flag(trap_exit, true),
    
    % Set up distributed Erlang
    case application:get_env(erlvectordb, cluster_enabled, false) of
        true ->
            setup_clustering(),
            % Start heartbeat timer
            HeartbeatInterval = application:get_env(erlvectordb, heartbeat_interval, 5000),
            timer:send_interval(HeartbeatInterval, heartbeat),
            
            ReplicationFactor = application:get_env(erlvectordb, replication_factor, 2),
            
            {ok, #state{
                replication_factor = ReplicationFactor,
                heartbeat_interval = HeartbeatInterval
            }};
        false ->
            {ok, #state{}}
    end.

handle_call({join_cluster, SeedNode}, _From, State) ->
    case net_adm:ping(SeedNode) of
        pong ->
            % Connect to seed node
            case rpc:call(SeedNode, cluster_manager, get_cluster_nodes, []) of
                {ok, ClusterNodes} ->
                    % Announce ourselves to the cluster
                    lists:foreach(fun(Node) ->
                        rpc:cast(Node, cluster_manager, node_joined, [node()])
                    end, ClusterNodes),
                    
                    NewNodes = lists:usort([SeedNode | ClusterNodes]),
                    
                    % Sync cluster state
                    sync_stores_from_cluster(NewNodes),
                    
                    {reply, {ok, joined}, State#state{
                        cluster_nodes = NewNodes,
                        node_status = active
                    }};
                Error ->
                    {reply, {error, {cluster_sync_failed, Error}}, State}
            end;
        pang ->
            {reply, {error, {seed_node_unreachable, SeedNode}}, State}
    end;

handle_call(leave_cluster, _From, State) ->
    % Announce leaving to cluster
    lists:foreach(fun(Node) ->
        rpc:cast(Node, cluster_manager, node_leaving, [node()])
    end, State#state.cluster_nodes),
    
    % Migrate stores to other nodes
    migrate_stores_before_leaving(State),
    
    {reply, ok, State#state{
        cluster_nodes = [],
        node_status = leaving,
        store_distribution = #{}
    }};

handle_call(get_cluster_nodes, _From, State) ->
    {reply, {ok, State#state.cluster_nodes}, State};

handle_call(get_node_status, _From, State) ->
    {reply, {ok, State#state.node_status}, State};

handle_call({distribute_store, StoreName, Options}, _From, State) ->
    ReplicationFactor = maps:get(replication_factor, Options, State#state.replication_factor),
    
    % Select nodes for store distribution
    AvailableNodes = [node() | State#state.cluster_nodes],
    SelectedNodes = select_nodes_for_store(AvailableNodes, ReplicationFactor),
    
    % Create store on selected nodes
    Results = lists:map(fun(Node) ->
        case Node of
            Node when Node =:= node() ->
                vector_store_sup:start_store(StoreName);
            _ ->
                rpc:call(Node, vector_store_sup, start_store, [StoreName])
        end
    end, SelectedNodes),
    
    case lists:all(fun({ok, _}) -> true; (_) -> false end, Results) of
        true ->
            NewDistribution = maps:put(StoreName, SelectedNodes, State#state.store_distribution),
            {reply, {ok, SelectedNodes}, State#state{store_distribution = NewDistribution}};
        false ->
            {reply, {error, {distribution_failed, Results}}, State}
    end;

handle_call({get_store_location, StoreName}, _From, State) ->
    case maps:get(StoreName, State#state.store_distribution, undefined) of
        undefined ->
            % Check if store exists locally
            case supervisor:which_children(vector_store_sup) of
                Children when is_list(Children) ->
                    case lists:keyfind(StoreName, 1, Children) of
                        {StoreName, _, _, _} -> {reply, {ok, [node()]}, State};
                        false -> {reply, {error, store_not_found}, State}
                    end;
                _ ->
                    {reply, {error, store_not_found}, State}
            end;
        Nodes ->
            {reply, {ok, Nodes}, State}
    end;

handle_call(sync_cluster_state, _From, State) ->
    % Sync store distribution across cluster
    ClusterState = collect_cluster_state(State#state.cluster_nodes),
    MergedDistribution = merge_store_distributions(ClusterState),
    
    {reply, ok, State#state{store_distribution = MergedDistribution}};

handle_call(get_cluster_stats, _From, State) ->
    Stats = #{
        cluster_nodes => length(State#state.cluster_nodes) + 1, % +1 for current node
        node_status => State#state.node_status,
        distributed_stores => maps:size(State#state.store_distribution),
        replication_factor => State#state.replication_factor,
        local_stores => length(supervisor:which_children(vector_store_sup))
    },
    {reply, {ok, Stats}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({node_joined, Node}, State) ->
    case lists:member(Node, State#state.cluster_nodes) of
        false ->
            NewNodes = [Node | State#state.cluster_nodes],
            monitor_node(Node, true),
            {noreply, State#state{cluster_nodes = NewNodes}};
        true ->
            {noreply, State}
    end;

handle_cast({node_leaving, Node}, State) ->
    NewNodes = lists:delete(Node, State#state.cluster_nodes),
    monitor_node(Node, false),
    
    % Handle store redistribution if needed
    NewDistribution = handle_node_leaving(Node, State#state.store_distribution),
    
    {noreply, State#state{
        cluster_nodes = NewNodes,
        store_distribution = NewDistribution
    }};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(heartbeat, State) ->
    % Send heartbeat to cluster nodes
    lists:foreach(fun(Node) ->
        rpc:cast(Node, cluster_manager, heartbeat_received, [node()])
    end, State#state.cluster_nodes),
    
    {noreply, State};

handle_info({nodedown, Node}, State) ->
    error_logger:warning_msg("Node ~p went down~n", [Node]),
    NewNodes = lists:delete(Node, State#state.cluster_nodes),
    
    % Handle store redistribution
    NewDistribution = handle_node_down(Node, State#state.store_distribution),
    
    {noreply, State#state{
        cluster_nodes = NewNodes,
        store_distribution = NewDistribution
    }};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    % Clean shutdown - notify cluster
    lists:foreach(fun(Node) ->
        rpc:cast(Node, cluster_manager, node_leaving, [node()])
    end, State#state.cluster_nodes),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
setup_clustering() ->
    % Set up distributed Erlang cookie if configured
    case application:get_env(erlvectordb, cluster_cookie, undefined) of
        undefined -> ok;
        Cookie -> erlang:set_cookie(node(), Cookie)
    end,
    
    % Enable distribution
    case application:get_env(erlvectordb, node_name, undefined) of
        undefined -> ok;
        NodeName -> 
            case net_kernel:start([NodeName, longnames]) of
                {ok, _} -> ok;
                {error, {already_started, _}} -> ok;
                Error -> 
                    error_logger:error_msg("Failed to start distribution: ~p~n", [Error])
            end
    end.

select_nodes_for_store(AvailableNodes, ReplicationFactor) ->
    % Simple round-robin selection for now
    % In production, this could consider node load, capacity, etc.
    ShuffledNodes = shuffle_list(AvailableNodes),
    lists:sublist(ShuffledNodes, min(ReplicationFactor, length(ShuffledNodes))).

shuffle_list(List) ->
    [X || {_, X} <- lists:sort([{rand:uniform(), Item} || Item <- List])].

sync_stores_from_cluster(ClusterNodes) ->
    % Get store information from cluster nodes
    lists:foreach(fun(Node) ->
        case rpc:call(Node, cluster_manager, get_store_distribution, []) of
            {ok, Distribution} ->
                % Replicate stores that should be on this node
                replicate_stores_locally(Distribution);
            _ ->
                ok
        end
    end, ClusterNodes).

replicate_stores_locally(Distribution) ->
    maps:foreach(fun(StoreName, Nodes) ->
        case lists:member(node(), Nodes) of
            true ->
                % This store should be replicated here
                case supervisor:which_children(vector_store_sup) of
                    Children when is_list(Children) ->
                        case lists:keyfind(StoreName, 1, Children) of
                            false ->
                                % Store doesn't exist locally, create it
                                vector_store_sup:start_store(StoreName);
                            _ ->
                                ok % Store already exists
                        end;
                    _ ->
                        ok
                end;
            false ->
                ok
        end
    end, Distribution).

migrate_stores_before_leaving(State) ->
    % Move stores to other nodes before leaving
    LocalStores = supervisor:which_children(vector_store_sup),
    
    lists:foreach(fun({StoreName, _, _, _}) ->
        case maps:get(StoreName, State#state.store_distribution, undefined) of
            undefined -> ok; % Local-only store
            Nodes ->
                OtherNodes = lists:delete(node(), Nodes),
                case OtherNodes of
                    [] ->
                        % Need to find new nodes for this store
                        migrate_store_to_new_nodes(StoreName, State#state.cluster_nodes);
                    _ ->
                        ok % Store exists on other nodes
                end
        end
    end, LocalStores).

migrate_store_to_new_nodes(StoreName, AvailableNodes) ->
    case AvailableNodes of
        [] ->
            error_logger:warning_msg("No available nodes to migrate store ~p~n", [StoreName]);
        _ ->
            % Select a node and migrate
            TargetNode = hd(AvailableNodes),
            case rpc:call(TargetNode, vector_store_sup, start_store, [StoreName]) of
                {ok, _} ->
                    % Sync data to target node
                    sync_store_data(StoreName, TargetNode);
                Error ->
                    error_logger:error_msg("Failed to migrate store ~p to ~p: ~p~n", 
                                         [StoreName, TargetNode, Error])
            end
    end.

sync_store_data(StoreName, TargetNode) ->
    % Get all vectors from local store
    case vector_store:get_all_vectors(StoreName) of
        {ok, Vectors} ->
            % Send vectors to target node
            lists:foreach(fun({VectorId, VectorData}) ->
                rpc:call(TargetNode, vector_store, insert, [StoreName, VectorId, VectorData])
            end, maps:to_list(Vectors));
        Error ->
            error_logger:error_msg("Failed to sync store data: ~p~n", [Error])
    end.

collect_cluster_state(ClusterNodes) ->
    lists:foldl(fun(Node, Acc) ->
        case rpc:call(Node, cluster_manager, get_store_distribution, []) of
            {ok, Distribution} -> [Distribution | Acc];
            _ -> Acc
        end
    end, [], ClusterNodes).

merge_store_distributions(ClusterStates) ->
    lists:foldl(fun(Distribution, Acc) ->
        maps:merge(Acc, Distribution)
    end, #{}, ClusterStates).

handle_node_leaving(Node, Distribution) ->
    % Remove node from store distributions and handle replication
    maps:map(fun(StoreName, Nodes) ->
        case lists:delete(Node, Nodes) of
            [] ->
                % No nodes left, store will be lost
                error_logger:warning_msg("Store ~p has no remaining replicas~n", [StoreName]),
                [];
            RemainingNodes ->
                RemainingNodes
        end
    end, Distribution).

handle_node_down(Node, Distribution) ->
    % Similar to handle_node_leaving but more urgent
    % Should trigger immediate re-replication
    NewDistribution = handle_node_leaving(Node, Distribution),
    
    % Trigger re-replication for under-replicated stores
    maps:foreach(fun(StoreName, Nodes) ->
        case length(Nodes) of
            0 -> 
                error_logger:error_msg("Store ~p lost all replicas due to node failure~n", [StoreName]);
            Count when Count < 2 -> % Assuming replication factor of 2
                error_logger:warning_msg("Store ~p is under-replicated (~p replicas)~n", 
                                       [StoreName, Count]);
            _ -> 
                ok
        end
    end, NewDistribution),
    
    NewDistribution.
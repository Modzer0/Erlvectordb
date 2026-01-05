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

-module(rest_api_server).

-export([start_link/0, handle_request/3]).

start_link() ->
    case application:get_env(erlvectordb, rest_api_enabled, false) of
        true ->
            Port = application:get_env(erlvectordb, rest_api_port, 8082),
            {ok, spawn_link(fun() -> start_server(Port) end)};
        false ->
            ignore
    end.

start_server(Port) ->
    case gen_tcp:listen(Port, [binary, {packet, http_bin}, {active, false}, {reuseaddr, true}]) of
        {ok, ListenSocket} ->
            error_logger:info_msg("REST API server listening on port ~p~n", [Port]),
            accept_loop(ListenSocket);
        {error, Reason} ->
            error_logger:error_msg("Failed to start REST API server: ~p~n", [Reason]),
            exit({rest_api_server_failed, Reason})
    end.

accept_loop(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            spawn(fun() -> handle_client(Socket) end),
            accept_loop(ListenSocket);
        {error, closed} ->
            ok;
        {error, Reason} ->
            error_logger:error_msg("REST API accept error: ~p~n", [Reason]),
            accept_loop(ListenSocket)
    end.

handle_client(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, {http_request, Method, {abs_path, Path}, _Version}} ->
            Headers = recv_headers(Socket, []),
            Body = recv_body(Socket, Headers),
            
            % Authenticate request
            case authenticate_rest_request(Headers) of
                {ok, ClientInfo} ->
                    Response = handle_request(Method, Path, #{
                        headers => Headers, 
                        body => Body, 
                        client_info => ClientInfo
                    }),
                    send_response(Socket, Response);
                {error, auth_error} ->
                    send_error_response(Socket, 401, <<"Unauthorized">>, 
                                      <<"Valid OAuth token required">>)
            end;
        {error, {http_error, _}} ->
            send_error_response(Socket, 400, <<"Bad Request">>, <<"Invalid HTTP request">>);
        {error, closed} ->
            ok;
        {error, Reason} ->
            error_logger:error_msg("REST API client error: ~p~n", [Reason])
    end,
    gen_tcp:close(Socket).

recv_headers(Socket, Headers) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, {http_header, _, Name, _, Value}} ->
            recv_headers(Socket, [{Name, Value} | Headers]);
        {ok, http_eoh} ->
            Headers;
        {error, _} ->
            Headers
    end.

recv_body(Socket, Headers) ->
    case lists:keyfind('Content-Length', 1, Headers) of
        {'Content-Length', LengthBin} ->
            Length = binary_to_integer(LengthBin),
            case gen_tcp:recv(Socket, Length) of
                {ok, Body} -> Body;
                {error, _} -> <<>>
            end;
        false ->
            <<>>
    end.

%% Store Management Endpoints
handle_request('POST', <<"/api/v1/stores">>, #{body := Body, client_info := ClientInfo}) ->
    case check_scope_permission([<<"write">>], ClientInfo) of
        true ->
            try
                #{<<"name">> := StoreName} = jsx:decode(Body, [return_maps]),
                StoreAtom = binary_to_atom(StoreName, utf8),
                
                case application:get_env(erlvectordb, cluster_enabled, false) of
                    true ->
                        case cluster_manager:distribute_store(StoreAtom, #{}) of
                            {ok, Nodes} ->
                                success_response(#{
                                    <<"store">> => StoreName,
                                    <<"nodes">> => [atom_to_binary(N, utf8) || N <- Nodes],
                                    <<"status">> => <<"created">>
                                });
                            {error, Reason} ->
                                error_response(500, <<"creation_failed">>, Reason)
                        end;
                    false ->
                        case vector_store_sup:start_store(StoreAtom) of
                            {ok, _Pid} ->
                                success_response(#{
                                    <<"store">> => StoreName,
                                    <<"status">> => <<"created">>
                                });
                            {error, Reason} ->
                                error_response(500, <<"creation_failed">>, Reason)
                        end
                end
            catch
                _:Error ->
                    error_response(400, <<"invalid_request">>, Error)
            end;
        false ->
            error_response(403, <<"insufficient_scope">>, <<"Write scope required">>)
    end;

handle_request('GET', <<"/api/v1/stores">>, #{client_info := ClientInfo}) ->
    case check_scope_permission([<<"read">>], ClientInfo) of
        true ->
            case application:get_env(erlvectordb, cluster_enabled, false) of
                true ->
                    {ok, ClusterStats} = cluster_manager:get_cluster_stats(),
                    LocalStores = supervisor:which_children(vector_store_sup),
                    StoreList = [#{
                        <<"name">> => atom_to_binary(Name, utf8),
                        <<"status">> => <<"active">>,
                        <<"distributed">> => true
                    } || {Name, _, _, _} <- LocalStores],
                    
                    success_response(#{
                        <<"stores">> => StoreList,
                        <<"cluster_info">> => ClusterStats
                    });
                false ->
                    LocalStores = supervisor:which_children(vector_store_sup),
                    StoreList = [#{
                        <<"name">> => atom_to_binary(Name, utf8),
                        <<"status">> => <<"active">>,
                        <<"distributed">> => false
                    } || {Name, _, _, _} <- LocalStores],
                    
                    success_response(#{<<"stores">> => StoreList})
            end;
        false ->
            error_response(403, <<"insufficient_scope">>, <<"Read scope required">>)
    end;

handle_request('DELETE', Path, #{client_info := ClientInfo}) ->
    case check_scope_permission([<<"admin">>], ClientInfo) of
        true ->
            case binary:split(Path, <<"/">>, [global]) of
                [<<>>, <<"api">>, <<"v1">>, <<"stores">>, StoreName] ->
                    StoreAtom = binary_to_atom(StoreName, utf8),
                    case vector_store_sup:stop_store(StoreAtom) of
                        ok ->
                            success_response(#{
                                <<"store">> => StoreName,
                                <<"status">> => <<"deleted">>
                            });
                        {error, Reason} ->
                            error_response(500, <<"deletion_failed">>, Reason)
                    end;
                _ ->
                    error_response(404, <<"not_found">>, <<"Invalid endpoint">>)
            end;
        false ->
            error_response(403, <<"insufficient_scope">>, <<"Admin scope required">>)
    end;

%% Vector Operations
handle_request('POST', Path, #{body := Body, client_info := ClientInfo}) ->
    case check_scope_permission([<<"write">>], ClientInfo) of
        true ->
            case binary:split(Path, <<"/">>, [global]) of
                [<<>>, <<"api">>, <<"v1">>, <<"stores">>, StoreName, <<"vectors">>] ->
                    handle_vector_insert(StoreName, Body);
                _ ->
                    error_response(404, <<"not_found">>, <<"Invalid endpoint">>)
            end;
        false ->
            error_response(403, <<"insufficient_scope">>, <<"Write scope required">>)
    end;

handle_request('GET', Path, #{client_info := ClientInfo}) ->
    case check_scope_permission([<<"read">>], ClientInfo) of
        true ->
            case binary:split(Path, <<"/">>, [global]) of
                [<<>>, <<"api">>, <<"v1">>, <<"stores">>, StoreName, <<"search">>] ->
                    % Handle search via query parameters (simplified)
                    error_response(501, <<"not_implemented">>, <<"Use POST for search">>);
                [<<>>, <<"api">>, <<"v1">>, <<"stores">>, StoreName, <<"stats">>] ->
                    handle_store_stats(StoreName);
                _ ->
                    error_response(404, <<"not_found">>, <<"Invalid endpoint">>)
            end;
        false ->
            error_response(403, <<"insufficient_scope">>, <<"Read scope required">>)
    end;

handle_request('POST', Path, #{body := Body, client_info := ClientInfo}) ->
    case check_scope_permission([<<"read">>], ClientInfo) of
        true ->
            case binary:split(Path, <<"/">>, [global]) of
                [<<>>, <<"api">>, <<"v1">>, <<"stores">>, StoreName, <<"search">>] ->
                    handle_vector_search(StoreName, Body);
                _ ->
                    error_response(404, <<"not_found">>, <<"Invalid endpoint">>)
            end;
        false ->
            error_response(403, <<"insufficient_scope">>, <<"Read scope required">>)
    end;

%% Cluster Management
handle_request('GET', <<"/api/v1/cluster/status">>, #{client_info := ClientInfo}) ->
    case check_scope_permission([<<"admin">>], ClientInfo) of
        true ->
            case application:get_env(erlvectordb, cluster_enabled, false) of
                true ->
                    {ok, ClusterStats} = cluster_manager:get_cluster_stats(),
                    {ok, ClusterNodes} = cluster_manager:get_cluster_nodes(),
                    {ok, NodeStatus} = cluster_manager:get_node_status(),
                    
                    success_response(#{
                        <<"cluster_enabled">> => true,
                        <<"current_node">> => atom_to_binary(node(), utf8),
                        <<"node_status">> => atom_to_binary(NodeStatus, utf8),
                        <<"cluster_nodes">> => [atom_to_binary(N, utf8) || N <- ClusterNodes],
                        <<"stats">> => ClusterStats
                    });
                false ->
                    success_response(#{
                        <<"cluster_enabled">> => false,
                        <<"current_node">> => atom_to_binary(node(), utf8)
                    })
            end;
        false ->
            error_response(403, <<"insufficient_scope">>, <<"Admin scope required">>)
    end;

handle_request('POST', <<"/api/v1/cluster/join">>, #{body := Body, client_info := ClientInfo}) ->
    case check_scope_permission([<<"admin">>], ClientInfo) of
        true ->
            try
                #{<<"seed_node">> := SeedNodeBin} = jsx:decode(Body, [return_maps]),
                SeedNode = binary_to_atom(SeedNodeBin, utf8),
                
                case cluster_manager:join_cluster(SeedNode) of
                    {ok, joined} ->
                        success_response(#{
                            <<"status">> => <<"joined">>,
                            <<"seed_node">> => SeedNodeBin
                        });
                    {error, Reason} ->
                        error_response(500, <<"join_failed">>, Reason)
                end
            catch
                _:Error ->
                    error_response(400, <<"invalid_request">>, Error)
            end;
        false ->
            error_response(403, <<"insufficient_scope">>, <<"Admin scope required">>)
    end;

handle_request('OPTIONS', _, _) ->
    cors_response();

handle_request(_, _, _) ->
    error_response(404, <<"not_found">>, <<"Endpoint not found">>).

%% Helper functions
handle_vector_insert(StoreName, Body) ->
    try
        #{<<"id">> := VectorId, <<"vector">> := Vector} = jsx:decode(Body, [return_maps]),
        Metadata = maps:get(<<"metadata">>, jsx:decode(Body, [return_maps]), #{}),
        
        StoreAtom = binary_to_atom(StoreName, utf8),
        VectorData = #{vector => Vector, metadata => Metadata},
        
        case vector_store:insert(StoreAtom, VectorId, VectorData) of
            ok ->
                success_response(#{
                    <<"id">> => VectorId,
                    <<"status">> => <<"inserted">>
                });
            {error, Reason} ->
                error_response(400, <<"insert_failed">>, Reason)
        end
    catch
        _:Error ->
            error_response(400, <<"invalid_request">>, Error)
    end.

handle_vector_search(StoreName, Body) ->
    try
        RequestData = jsx:decode(Body, [return_maps]),
        #{<<"vector">> := QueryVector} = RequestData,
        K = maps:get(<<"k">>, RequestData, 10),
        
        StoreAtom = binary_to_atom(StoreName, utf8),
        
        case vector_store:search(StoreAtom, QueryVector, K) of
            {ok, Results} ->
                FormattedResults = [#{
                    <<"id">> => Id,
                    <<"metadata">> => Metadata,
                    <<"distance">> => Distance
                } || {Id, Metadata, Distance} <- Results],
                
                success_response(#{
                    <<"results">> => FormattedResults,
                    <<"count">> => length(FormattedResults)
                });
            {error, Reason} ->
                error_response(400, <<"search_failed">>, Reason)
        end
    catch
        _:Error ->
            error_response(400, <<"invalid_request">>, Error)
    end.

handle_store_stats(StoreName) ->
    try
        StoreAtom = binary_to_atom(StoreName, utf8),
        case vector_store:get_stats(StoreAtom) of
            {ok, Stats} ->
                success_response(Stats);
            {error, Reason} ->
                error_response(404, <<"store_not_found">>, Reason)
        end
    catch
        _:Error ->
            error_response(400, <<"invalid_request">>, Error)
    end.

authenticate_rest_request(Headers) ->
    case application:get_env(erlvectordb, oauth_enabled, true) of
        false ->
            {ok, #{client_id => <<"anonymous">>, scopes => [<<"read">>, <<"write">>, <<"admin">>]}};
        true ->
            case lists:keyfind('Authorization', 1, Headers) of
                {'Authorization', <<"Bearer ", Token/binary>>} ->
                    case oauth_server:validate_token(Token) of
                        {ok, ClientInfo} -> {ok, ClientInfo};
                        {error, _} -> {error, auth_error}
                    end;
                _ ->
                    {error, auth_error}
            end
    end.

check_scope_permission(RequiredScopes, ClientInfo) ->
    ClientScopes = maps:get(scopes, ClientInfo, []),
    lists:any(fun(RequiredScope) ->
        lists:member(RequiredScope, ClientScopes)
    end, RequiredScopes).

success_response(Data) ->
    Body = jsx:encode(Data),
    {200, [
        {<<"Content-Type">>, <<"application/json">>},
        {<<"Access-Control-Allow-Origin">>, <<"*">>},
        {<<"Access-Control-Allow-Methods">>, <<"GET, POST, PUT, DELETE, OPTIONS">>},
        {<<"Access-Control-Allow-Headers">>, <<"Authorization, Content-Type">>}
    ], Body}.

error_response(Status, Error, Description) ->
    Body = jsx:encode(#{
        <<"error">> => Error,
        <<"message">> => iolist_to_binary(io_lib:format("~p", [Description]))
    }),
    {Status, [
        {<<"Content-Type">>, <<"application/json">>},
        {<<"Access-Control-Allow-Origin">>, <<"*">>}
    ], Body}.

cors_response() ->
    {200, [
        {<<"Access-Control-Allow-Origin">>, <<"*">>},
        {<<"Access-Control-Allow-Methods">>, <<"GET, POST, PUT, DELETE, OPTIONS">>},
        {<<"Access-Control-Allow-Headers">>, <<"Authorization, Content-Type">>},
        {<<"Access-Control-Max-Age">>, <<"86400">>}
    ], <<>>}.

send_response(Socket, {Status, Headers, Body}) ->
    StatusLine = io_lib:format("HTTP/1.1 ~w ~s\r\n", [Status, status_text(Status)]),
    HeaderLines = [[atom_to_list(Name), ": ", Value, "\r\n"] || {Name, Value} <- Headers],
    Response = [StatusLine, HeaderLines, "\r\n", Body],
    gen_tcp:send(Socket, Response).

send_error_response(Socket, Status, Error, Message) ->
    Body = jsx:encode(#{<<"error">> => Error, <<"message">> => Message}),
    send_response(Socket, {Status, [{<<"Content-Type">>, <<"application/json">>}], Body}).

status_text(200) -> "OK";
status_text(400) -> "Bad Request";
status_text(401) -> "Unauthorized";
status_text(403) -> "Forbidden";
status_text(404) -> "Not Found";
status_text(500) -> "Internal Server Error";
status_text(501) -> "Not Implemented";
status_text(_) -> "Unknown".
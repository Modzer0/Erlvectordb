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

-module(mcp_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    socket :: inet:socket() | undefined,
    port = 8080 :: integer()
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Port = application:get_env(erlvectordb, mcp_port, 8080),
    case gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]) of
        {ok, Socket} ->
            spawn_link(fun() -> accept_loop(Socket) end),
            {ok, #state{socket = Socket, port = Port}};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{socket = Socket}) ->
    case Socket of
        undefined -> ok;
        _ -> gen_tcp:close(Socket)
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
accept_loop(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            spawn(fun() -> handle_client(Socket) end),
            accept_loop(ListenSocket);
        {error, closed} ->
            ok;
        {error, Reason} ->
            error_logger:error_msg("Accept error: ~p~n", [Reason]),
            accept_loop(ListenSocket)
    end.

handle_client(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            try
                Request = jsx:decode(Data, [return_maps]),
                Response = process_mcp_request(Request),
                ResponseJson = jsx:encode(Response),
                gen_tcp:send(Socket, ResponseJson),
                handle_client(Socket)
            catch
                _:Error ->
                    ErrorResponse = #{
                        <<"jsonrpc">> => <<"2.0">>,
                        <<"error">> => #{
                            <<"code">> => -32700,
                            <<"message">> => <<"Parse error">>,
                            <<"data">> => list_to_binary(io_lib:format("~p", [Error]))
                        },
                        <<"id">> => null
                    },
                    gen_tcp:send(Socket, jsx:encode(ErrorResponse))
            end;
        {error, closed} ->
            ok;
        {error, Reason} ->
            error_logger:error_msg("Client error: ~p~n", [Reason])
    end,
    gen_tcp:close(Socket).

process_mcp_request(#{<<"method">> := <<"initialize">>, <<"id">> := Id}) ->
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"result">> => #{
            <<"protocolVersion">> => <<"2024-11-05">>,
            <<"capabilities">> => #{
                <<"tools">> => #{},
                <<"resources">> => #{}
            },
            <<"serverInfo">> => #{
                <<"name">> => <<"erlvectordb">>,
                <<"version">> => <<"0.1.0">>
            }
        },
        <<"id">> => Id
    };

process_mcp_request(#{<<"method">> := <<"tools/list">>, <<"id">> := Id}) ->
    Tools = [
        #{
            <<"name">> => <<"create_store">>,
            <<"description">> => <<"Create a new vector store">>,
            <<"inputSchema">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"name">> => #{<<"type">> => <<"string">>}
                },
                <<"required">> => [<<"name">>]
            }
        },
        #{
            <<"name">> => <<"insert_vector">>,
            <<"description">> => <<"Insert a vector into a store">>,
            <<"inputSchema">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"store">> => #{<<"type">> => <<"string">>},
                    <<"id">> => #{<<"type">> => <<"string">>},
                    <<"vector">> => #{
                        <<"type">> => <<"array">>,
                        <<"items">> => #{<<"type">> => <<"number">>}
                    },
                    <<"metadata">> => #{<<"type">> => <<"object">>}
                },
                <<"required">> => [<<"store">>, <<"id">>, <<"vector">>]
            }
        },
        #{
            <<"name">> => <<"search_vectors">>,
            <<"description">> => <<"Search for similar vectors">>,
            <<"inputSchema">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"store">> => #{<<"type">> => <<"string">>},
                    <<"vector">> => #{
                        <<"type">> => <<"array">>,
                        <<"items">> => #{<<"type">> => <<"number">>}
                    },
                    <<"k">> => #{<<"type">> => <<"integer">>, <<"default">> => 10}
                },
                <<"required">> => [<<"store">>, <<"vector">>]
            }
        },
        #{
            <<"name">> => <<"sync_store">>,
            <<"description">> => <<"Sync a vector store to persistent storage">>,
            <<"inputSchema">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"store">> => #{<<"type">> => <<"string">>}
                },
                <<"required">> => [<<"store">>]
            }
        },
        #{
            <<"name">> => <<"backup_store">>,
            <<"description">> => <<"Create a backup of a vector store">>,
            <<"inputSchema">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"store">> => #{<<"type">> => <<"string">>},
                    <<"backup_name">> => #{<<"type">> => <<"string">>}
                },
                <<"required">> => [<<"store">>, <<"backup_name">>]
            }
        },
        #{
            <<"name">> => <<"restore_store">>,
            <<"description">> => <<"Restore a vector store from backup">>,
            <<"inputSchema">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"backup_path">> => #{<<"type">> => <<"string">>},
                    <<"new_store_name">> => #{<<"type">> => <<"string">>}
                },
                <<"required">> => [<<"backup_path">>, <<"new_store_name">>]
            }
        },
        #{
            <<"name">> => <<"list_backups">>,
            <<"description">> => <<"List all available backups">>,
            <<"inputSchema">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{},
                <<"required">> => []
            }
        }
    ],
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"result">> => #{<<"tools">> => Tools},
        <<"id">> => Id
    };

process_mcp_request(#{<<"method">> := <<"tools/call">>, <<"params">> := Params, <<"id">> := Id}) ->
    Result = handle_tool_call(Params),
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"result">> => Result,
        <<"id">> => Id
    };

process_mcp_request(#{<<"id">> := Id}) ->
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"error">> => #{
            <<"code">> => -32601,
            <<"message">> => <<"Method not found">>
        },
        <<"id">> => Id
    }.

handle_tool_call(#{<<"name">> := <<"create_store">>, <<"arguments">> := Args}) ->
    StoreName = binary_to_atom(maps:get(<<"name">>, Args), utf8),
    case vector_store_sup:start_store(StoreName) of
        {ok, _Pid} ->
            #{<<"content">> => [#{<<"type">> => <<"text">>, <<"text">> => <<"Store created successfully">>}]};
        {error, Reason} ->
            #{<<"isError">> => true, <<"content">> => [#{<<"type">> => <<"text">>, <<"text">> => io_lib:format("Error: ~p", [Reason])}]}
    end;

handle_tool_call(#{<<"name">> := <<"insert_vector">>, <<"arguments">> := Args}) ->
    StoreName = binary_to_atom(maps:get(<<"store">>, Args), utf8),
    VectorId = maps:get(<<"id">>, Args),
    Vector = maps:get(<<"vector">>, Args),
    Metadata = maps:get(<<"metadata">>, Args, #{}),
    
    VectorData = #{vector => Vector, metadata => Metadata},
    case vector_store:insert(StoreName, VectorId, VectorData) of
        ok ->
            #{<<"content">> => [#{<<"type">> => <<"text">>, <<"text">> => <<"Vector inserted successfully">>}]};
        {error, Reason} ->
            #{<<"isError">> => true, <<"content">> => [#{<<"type">> => <<"text">>, <<"text">> => io_lib:format("Error: ~p", [Reason])}]}
    end;

handle_tool_call(#{<<"name">> := <<"search_vectors">>, <<"arguments">> := Args}) ->
    StoreName = binary_to_atom(maps:get(<<"store">>, Args), utf8),
    QueryVector = maps:get(<<"vector">>, Args),
    K = maps:get(<<"k">>, Args, 10),
    
    case vector_store:search(StoreName, QueryVector, K) of
        {ok, Results} ->
            ResultsJson = jsx:encode(Results),
            #{<<"content">> => [#{<<"type">> => <<"text">>, <<"text">> => ResultsJson}]};
        {error, Reason} ->
            #{<<"isError">> => true, <<"content">> => [#{<<"type">> => <<"text">>, <<"text">> => io_lib:format("Error: ~p", [Reason])}]}
    end;

handle_tool_call(#{<<"name">> := <<"sync_store">>, <<"arguments">> := Args}) ->
    StoreName = binary_to_atom(maps:get(<<"store">>, Args), utf8),
    
    case vector_store:sync(StoreName) of
        ok ->
            #{<<"content">> => [#{<<"type">> => <<"text">>, <<"text">> => <<"Store synced successfully">>}]};
        {error, Reason} ->
            #{<<"isError">> => true, <<"content">> => [#{<<"type">> => <<"text">>, <<"text">> => io_lib:format("Error: ~p", [Reason])}]}
    end;

handle_tool_call(#{<<"name">> := <<"backup_store">>, <<"arguments">> := Args}) ->
    StoreName = binary_to_atom(maps:get(<<"store">>, Args), utf8),
    BackupName = binary_to_list(maps:get(<<"backup_name">>, Args)),
    
    case vector_backup:backup_store(StoreName, BackupName) of
        {ok, BackupInfo} ->
            Response = io_lib:format("Backup created successfully: ~s", [BackupInfo#backup_info.file_path]),
            #{<<"content">> => [#{<<"type">> => <<"text">>, <<"text">> => list_to_binary(Response)}]};
        {error, Reason} ->
            #{<<"isError">> => true, <<"content">> => [#{<<"type">> => <<"text">>, <<"text">> => io_lib:format("Error: ~p", [Reason])}]}
    end;

handle_tool_call(#{<<"name">> := <<"restore_store">>, <<"arguments">> := Args}) ->
    BackupPath = binary_to_list(maps:get(<<"backup_path">>, Args)),
    NewStoreName = binary_to_atom(maps:get(<<"new_store_name">>, Args), utf8),
    
    case vector_backup:restore_store(BackupPath, NewStoreName) of
        {ok, Result} ->
            VectorsRestored = maps:get(vectors_restored, Result),
            Response = io_lib:format("Store restored successfully. Vectors restored: ~p", [VectorsRestored]),
            #{<<"content">> => [#{<<"type">> => <<"text">>, <<"text">> => list_to_binary(Response)}]};
        {error, Reason} ->
            #{<<"isError">> => true, <<"content">> => [#{<<"type">> => <<"text">>, <<"text">> => io_lib:format("Error: ~p", [Reason])}]}
    end;

handle_tool_call(#{<<"name">> := <<"list_backups">>, <<"arguments">> := _Args}) ->
    case vector_backup:list_backups() of
        {ok, Backups} ->
            BackupList = [#{
                <<"store_name">> => atom_to_binary(B#backup_info.store_name, utf8),
                <<"timestamp">> => B#backup_info.timestamp,
                <<"file_path">> => list_to_binary(B#backup_info.file_path),
                <<"vector_count">> => B#backup_info.vector_count,
                <<"dimension">> => B#backup_info.dimension
            } || B <- Backups],
            ResultsJson = jsx:encode(BackupList),
            #{<<"content">> => [#{<<"type">> => <<"text">>, <<"text">> => ResultsJson}]};
        {error, Reason} ->
            #{<<"isError">> => true, <<"content">> => [#{<<"type">> => <<"text">>, <<"text">> => io_lib:format("Error: ~p", [Reason])}]}
    end;

handle_tool_call(_) ->
    #{<<"isError">> => true, <<"content">> => [#{<<"type">> => <<"text">>, <<"text">> => <<"Unknown tool">>}]}.

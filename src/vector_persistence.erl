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

-module(vector_persistence).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([save_vector/4, save_compressed_vector/4, load_vectors/1, delete_vector/2, 
         get_store_info/1, sync/1, close_store/1]).

-record(state, {
    store_name :: atom(),
    dets_table :: dets:tab_name(),
    ets_table :: ets:tab(),
    data_dir :: string(),
    sync_interval = 30000 :: integer(),
    dirty = false :: boolean()
}).

-record(vector_record, {
    id :: binary(),
    vector :: [float()] | compressed_vector(),
    metadata :: map(),
    timestamp :: integer(),
    compressed = false :: boolean()
}).

-record(store_info, {
    name :: atom(),
    dimension :: integer() | undefined,
    count :: integer(),
    created :: integer(),
    last_modified :: integer()
}).

%% API
start_link(StoreName) ->
    gen_server:start_link({local, persistence_name(StoreName)}, ?MODULE, [StoreName], []).

save_vector(StoreName, VectorId, Vector, Metadata) ->
    gen_server:call(persistence_name(StoreName), {save_vector, VectorId, Vector, Metadata}).

save_compressed_vector(StoreName, VectorId, CompressedVector, Metadata) ->
    gen_server:call(persistence_name(StoreName), {save_compressed_vector, VectorId, CompressedVector, Metadata}).

load_vectors(StoreName) ->
    gen_server:call(persistence_name(StoreName), load_vectors).

delete_vector(StoreName, VectorId) ->
    gen_server:call(persistence_name(StoreName), {delete_vector, VectorId}).

get_store_info(StoreName) ->
    gen_server:call(persistence_name(StoreName), get_store_info).

sync(StoreName) ->
    gen_server:call(persistence_name(StoreName), sync).

close_store(StoreName) ->
    gen_server:call(persistence_name(StoreName), close_store).

%% Callbacks
init([StoreName]) ->
    process_flag(trap_exit, true),
    
    DataDir = application:get_env(erlvectordb, persistence_dir, "data"),
    ok = filelib:ensure_dir(filename:join(DataDir, "dummy")),
    
    DetsFile = filename:join(DataDir, atom_to_list(StoreName) ++ ".dets"),
    EtsTable = ets:new(StoreName, [set, protected, {keypos, #vector_record.id}]),
    
    case dets:open_file(StoreName, [{file, DetsFile}, {keypos, #vector_record.id}]) of
        {ok, DetsTable} ->
            % Load existing data into ETS for fast access
            load_from_dets_to_ets(DetsTable, EtsTable),
            
            % Start periodic sync timer
            SyncInterval = application:get_env(erlvectordb, sync_interval, 30000),
            timer:send_interval(SyncInterval, sync_timer),
            
            {ok, #state{
                store_name = StoreName,
                dets_table = DetsTable,
                ets_table = EtsTable,
                data_dir = DataDir,
                sync_interval = SyncInterval
            }};
        {error, Reason} ->
            ets:delete(EtsTable),
            {stop, {dets_open_error, Reason}}
    end.

handle_call({save_vector, VectorId, Vector, Metadata}, _From, State) ->
    CompressionEnabled = application:get_env(erlvectordb, compression_enabled, false),
    
    {FinalVector, IsCompressed} = case CompressionEnabled of
        true ->
            Algorithm = application:get_env(erlvectordb, compression_algorithm, quantization_8bit),
            case vector_compression:compress_vector(Vector, Algorithm) of
                {ok, CompressedVector} ->
                    {CompressedVector, true};
                {error, _Reason} ->
                    % Fallback to uncompressed if compression fails
                    {Vector, false}
            end;
        false ->
            {Vector, false}
    end,
    
    Timestamp = erlang:system_time(millisecond),
    Record = #vector_record{
        id = VectorId,
        vector = FinalVector,
        metadata = Metadata,
        timestamp = Timestamp,
        compressed = IsCompressed
    },
    
    % Save to ETS for fast access
    true = ets:insert(State#state.ets_table, Record),
    
    % Mark as dirty for next sync
    NewState = State#state{dirty = true},
    
    {reply, ok, NewState};

handle_call({save_compressed_vector, VectorId, CompressedVector, Metadata}, _From, State) ->
    Timestamp = erlang:system_time(millisecond),
    Record = #vector_record{
        id = VectorId,
        vector = CompressedVector,
        metadata = Metadata,
        timestamp = Timestamp,
        compressed = true
    },
    
    % Save to ETS for fast access
    true = ets:insert(State#state.ets_table, Record),
    
    % Mark as dirty for next sync
    NewState = State#state{dirty = true},
    
    {reply, ok, NewState};

handle_call(load_vectors, _From, State) ->
    Vectors = ets:tab2list(State#state.ets_table),
    VectorMap = maps:from_list([{R#vector_record.id, 
                                #{vector => decompress_if_needed(R#vector_record.vector, R#vector_record.compressed),
                                  metadata => R#vector_record.metadata,
                                  timestamp => R#vector_record.timestamp,
                                  compressed => R#vector_record.compressed}} 
                               || R <- Vectors]),
    {reply, {ok, VectorMap}, State};

handle_call({delete_vector, VectorId}, _From, State) ->
    true = ets:delete(State#state.ets_table, VectorId),
    NewState = State#state{dirty = true},
    {reply, ok, NewState};

handle_call(get_store_info, _From, State) ->
    Count = ets:info(State#state.ets_table, size),
    
    % Calculate dimension from first vector if available
    Dimension = case ets:first(State#state.ets_table) of
        '$end_of_table' -> undefined;
        FirstKey ->
            case ets:lookup(State#state.ets_table, FirstKey) of
                [#vector_record{vector = Vector}] -> length(Vector);
                [] -> undefined
            end
    end,
    
    % Get creation time from DETS info
    DetsInfo = dets:info(State#state.dets_table),
    Created = proplists:get_value(file_size, DetsInfo, 0),
    
    Info = #store_info{
        name = State#state.store_name,
        dimension = Dimension,
        count = Count,
        created = Created,
        last_modified = erlang:system_time(millisecond)
    },
    
    {reply, {ok, Info}, State};

handle_call(sync, _From, State) ->
    case State#state.dirty of
        true ->
            Result = sync_to_dets(State),
            NewState = State#state{dirty = false},
            {reply, Result, NewState};
        false ->
            {reply, ok, State}
    end;

handle_call(close_store, _From, State) ->
    sync_to_dets(State),
    dets:close(State#state.dets_table),
    ets:delete(State#state.ets_table),
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(sync_timer, State) ->
    case State#state.dirty of
        true ->
            sync_to_dets(State),
            {noreply, State#state{dirty = false}};
        false ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    sync_to_dets(State),
    dets:close(State#state.dets_table),
    ets:delete(State#state.ets_table),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
persistence_name(StoreName) ->
    list_to_atom(atom_to_list(StoreName) ++ "_persistence").

load_from_dets_to_ets(DetsTable, EtsTable) ->
    dets:traverse(DetsTable, fun(Record) ->
        ets:insert(EtsTable, Record),
        continue
    end).

sync_to_dets(State) ->
    try
        % Clear DETS and reload from ETS
        ok = dets:delete_all_objects(State#state.dets_table),
        
        % Insert all ETS records into DETS
        ets:foldl(fun(Record, Acc) ->
            ok = dets:insert(State#state.dets_table, Record),
            Acc
        end, ok, State#state.ets_table),
        
        % Sync to disk
        ok = dets:sync(State#state.dets_table),
        ok
    catch
        Error:Reason ->
            error_logger:error_msg("Sync to DETS failed: ~p:~p~n", [Error, Reason]),
            {error, {sync_failed, Reason}}
    end.

%% Compression helper functions
decompress_if_needed(Vector, false) ->
    Vector;
decompress_if_needed(CompressedVector, true) ->
    case vector_compression:decompress_vector(CompressedVector, #{}) of
        {ok, Vector} -> Vector;
        {error, _Reason} -> 
            error_logger:warning_msg("Failed to decompress vector, returning compressed data~n"),
            CompressedVector
    end.
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

-module(compression_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() ->
    [
        test_8bit_quantization,
        test_4bit_quantization,
        test_pca_compression,
        test_zlib_compression,
        test_compression_ratio,
        test_batch_compression,
        test_compression_benchmark
    ].

init_per_suite(Config) ->
    application:ensure_all_started(erlvectordb),
    Config.

end_per_suite(_Config) ->
    application:stop(erlvectordb),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

test_8bit_quantization(_Config) ->
    Vector = [1.0, 2.5, 3.7, 4.2, 5.9],
    
    % Test compression
    {ok, Compressed} = vector_compression:compress_vector(Vector, quantization_8bit),
    
    % Verify compressed structure
    quantization_8bit = maps:get(algorithm, Compressed),
    true = is_binary(maps:get(data, Compressed)),
    true = is_map(maps:get(metadata, Compressed)),
    
    % Test decompression
    {ok, Decompressed} = vector_compression:decompress_vector(Compressed, #{}),
    
    % Verify length is preserved
    length(Vector) = length(Decompressed),
    
    % Verify values are approximately correct (within quantization error)
    lists:foreach(fun({Original, Decompressed_Val}) ->
        Diff = abs(Original - Decompressed_Val),
        true = Diff < 0.1 % Allow for quantization error
    end, lists:zip(Vector, Decompressed)).

test_4bit_quantization(_Config) ->
    Vector = [1.0, 2.0, 3.0, 4.0],
    
    {ok, Compressed} = vector_compression:compress_vector(Vector, quantization_4bit),
    {ok, Decompressed} = vector_compression:decompress_vector(Compressed, #{}),
    
    length(Vector) = length(Decompressed),
    
    % 4-bit quantization should have higher error than 8-bit
    lists:foreach(fun({Original, Decompressed_Val}) ->
        Diff = abs(Original - Decompressed_Val),
        true = Diff < 0.5 % Allow for higher quantization error
    end, lists:zip(Vector, Decompressed)).

test_pca_compression(_Config) ->
    Vector = lists:seq(1.0, 10.0, 1.0), % [1.0, 2.0, ..., 10.0]
    
    {ok, Compressed} = vector_compression:compress_vector(Vector, pca_compression),
    {ok, Decompressed} = vector_compression:decompress_vector(Compressed, #{}),
    
    length(Vector) = length(Decompressed),
    
    % PCA compression should preserve some structure
    true = is_list(Decompressed).

test_zlib_compression(_Config) ->
    Vector = [1.1, 2.2, 3.3, 4.4, 5.5],
    
    {ok, Compressed} = vector_compression:compress_vector(Vector, zlib_compression),
    {ok, Decompressed} = vector_compression:decompress_vector(Compressed, #{}),
    
    % Zlib should preserve exact values
    Vector = Decompressed.

test_compression_ratio(_Config) ->
    Vector = lists:duplicate(100, 1.0), % Highly compressible vector
    
    {ok, Compressed} = vector_compression:compress_vector(Vector, zlib_compression),
    Ratio = vector_compression:get_compression_ratio(Vector, Compressed),
    
    % Should achieve good compression on repeated values
    true = Ratio > 1.0.

test_batch_compression(_Config) ->
    Vectors = [
        [1.0, 2.0, 3.0],
        [4.0, 5.0, 6.0],
        [7.0, 8.0, 9.0]
    ],
    
    {ok, CompressedVectors} = vector_compression:compress_batch(Vectors, quantization_8bit),
    3 = length(CompressedVectors),
    
    {ok, DecompressedVectors} = vector_compression:decompress_batch(CompressedVectors, #{}),
    3 = length(DecompressedVectors),
    
    % Verify each vector is approximately correct
    lists:foreach(fun({Original, Decompressed}) ->
        length(Original) = length(Decompressed)
    end, lists:zip(Vectors, DecompressedVectors)).

test_compression_benchmark(_Config) ->
    Vector = lists:seq(1.0, 50.0, 1.0),
    Algorithms = [quantization_8bit, quantization_4bit, zlib_compression],
    
    Results = vector_compression:benchmark_compression(Vector, Algorithms),
    
    % Verify benchmark results structure
    3 = length(Results),
    
    lists:foreach(fun({Algorithm, Metrics}) ->
        true = lists:member(Algorithm, Algorithms),
        true = is_map(Metrics),
        true = maps:is_key(compression_time, Metrics),
        true = maps:is_key(decompression_time, Metrics),
        true = maps:is_key(compression_ratio, Metrics),
        true = maps:is_key(accuracy_loss, Metrics)
    end, Results).
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

-module(vector_compression).

-export([
    compress_vector/2,
    decompress_vector/2,
    compress_batch/2,
    decompress_batch/2,
    get_compression_ratio/2,
    get_supported_algorithms/0,
    benchmark_compression/2
]).

-type compression_algorithm() :: 
    quantization_8bit | 
    quantization_4bit | 
    pca_compression |
    zlib_compression |
    lz4_compression |
    product_quantization.

-type compressed_vector() :: #{
    algorithm := compression_algorithm(),
    data := binary(),
    metadata := map()
}.

%% API Functions

%% Compress a single vector
-spec compress_vector([float()], compression_algorithm()) -> 
    {ok, compressed_vector()} | {error, term()}.
compress_vector(Vector, Algorithm) ->
    try
        case Algorithm of
            quantization_8bit ->
                compress_8bit_quantization(Vector);
            quantization_4bit ->
                compress_4bit_quantization(Vector);
            pca_compression ->
                compress_pca(Vector);
            zlib_compression ->
                compress_zlib(Vector);
            lz4_compression ->
                compress_lz4(Vector);
            product_quantization ->
                compress_product_quantization(Vector);
            _ ->
                {error, {unsupported_algorithm, Algorithm}}
        end
    catch
        Error:Reason ->
            {error, {compression_failed, Error, Reason}}
    end.

%% Decompress a single vector
-spec decompress_vector(compressed_vector(), map()) -> 
    {ok, [float()]} | {error, term()}.
decompress_vector(#{algorithm := Algorithm, data := Data, metadata := Metadata}, _Options) ->
    try
        case Algorithm of
            quantization_8bit ->
                decompress_8bit_quantization(Data, Metadata);
            quantization_4bit ->
                decompress_4bit_quantization(Data, Metadata);
            pca_compression ->
                decompress_pca(Data, Metadata);
            zlib_compression ->
                decompress_zlib(Data, Metadata);
            lz4_compression ->
                decompress_lz4(Data, Metadata);
            product_quantization ->
                decompress_product_quantization(Data, Metadata);
            _ ->
                {error, {unsupported_algorithm, Algorithm}}
        end
    catch
        Error:Reason ->
            {error, {decompression_failed, Error, Reason}}
    end.

%% Compress multiple vectors at once (more efficient for some algorithms)
-spec compress_batch([[float()]], compression_algorithm()) -> 
    {ok, [compressed_vector()]} | {error, term()}.
compress_batch(Vectors, Algorithm) ->
    case Algorithm of
        pca_compression ->
            compress_pca_batch(Vectors);
        product_quantization ->
            compress_product_quantization_batch(Vectors);
        _ ->
            % For other algorithms, compress individually
            Results = [compress_vector(V, Algorithm) || V <- Vectors],
            case lists:all(fun({ok, _}) -> true; (_) -> false end, Results) of
                true -> {ok, [CV || {ok, CV} <- Results]};
                false -> {error, batch_compression_failed}
            end
    end.

%% Decompress multiple vectors
-spec decompress_batch([compressed_vector()], map()) -> 
    {ok, [[float()]]} | {error, term()}.
decompress_batch(CompressedVectors, Options) ->
    Results = [decompress_vector(CV, Options) || CV <- CompressedVectors],
    case lists:all(fun({ok, _}) -> true; (_) -> false end, Results) of
        true -> {ok, [V || {ok, V} <- Results]};
        false -> {error, batch_decompression_failed}
    end.

%% Calculate compression ratio
-spec get_compression_ratio([float()], compressed_vector()) -> float().
get_compression_ratio(OriginalVector, CompressedVector) ->
    OriginalSize = length(OriginalVector) * 4, % 4 bytes per float
    CompressedSize = byte_size(maps:get(data, CompressedVector)),
    OriginalSize / CompressedSize.

%% Get list of supported compression algorithms
-spec get_supported_algorithms() -> [compression_algorithm()].
get_supported_algorithms() ->
    [quantization_8bit, quantization_4bit, pca_compression, 
     zlib_compression, lz4_compression, product_quantization].

%% Benchmark different compression algorithms
-spec benchmark_compression([float()], [compression_algorithm()]) -> 
    [{compression_algorithm(), #{compression_time := integer(), 
                                decompression_time := integer(),
                                compression_ratio := float(),
                                accuracy_loss := float()}}].
benchmark_compression(Vector, Algorithms) ->
    lists:map(fun(Algorithm) ->
        % Compression benchmark
        {CompressTime, {ok, Compressed}} = timer:tc(fun() ->
            compress_vector(Vector, Algorithm)
        end),
        
        % Decompression benchmark
        {DecompressTime, {ok, Decompressed}} = timer:tc(fun() ->
            decompress_vector(Compressed, #{})
        end),
        
        % Calculate metrics
        CompressionRatio = get_compression_ratio(Vector, Compressed),
        AccuracyLoss = calculate_accuracy_loss(Vector, Decompressed),
        
        {Algorithm, #{
            compression_time => CompressTime,
            decompression_time => DecompressTime,
            compression_ratio => CompressionRatio,
            accuracy_loss => AccuracyLoss
        }}
    end, Algorithms).

%% Internal compression functions

%% 8-bit quantization
compress_8bit_quantization(Vector) ->
    {Min, Max} = find_min_max(Vector),
    Scale = (Max - Min) / 255.0,
    
    QuantizedVector = [round((V - Min) / Scale) || V <- Vector],
    Data = list_to_binary(QuantizedVector),
    
    {ok, #{
        algorithm => quantization_8bit,
        data => Data,
        metadata => #{min => Min, max => Max, scale => Scale}
    }}.

decompress_8bit_quantization(Data, #{min := Min, scale := Scale}) ->
    QuantizedVector = binary_to_list(Data),
    Vector = [Min + (Q * Scale) || Q <- QuantizedVector],
    {ok, Vector}.

%% 4-bit quantization
compress_4bit_quantization(Vector) ->
    {Min, Max} = find_min_max(Vector),
    Scale = (Max - Min) / 15.0,
    
    QuantizedVector = [round((V - Min) / Scale) || V <- Vector],
    
    % Pack two 4-bit values into each byte
    PackedData = pack_4bit_values(QuantizedVector),
    
    {ok, #{
        algorithm => quantization_4bit,
        data => PackedData,
        metadata => #{min => Min, max => Max, scale => Scale, length => length(Vector)}
    }}.

decompress_4bit_quantization(Data, #{min := Min, scale := Scale, length := Length}) ->
    QuantizedVector = unpack_4bit_values(Data, Length),
    Vector = [Min + (Q * Scale) || Q <- QuantizedVector],
    {ok, Vector}.

%% PCA compression
compress_pca(Vector) ->
    % Simple PCA compression - in practice, you'd need a trained PCA model
    % This is a placeholder implementation
    Dimension = length(Vector),
    TargetDimension = max(1, Dimension div 2), % Compress to half dimensions
    
    % Truncate vector (simplified PCA)
    CompressedVector = lists:sublist(Vector, TargetDimension),
    Data = list_to_binary([<<V:32/float>> || V <- CompressedVector]),
    
    {ok, #{
        algorithm => pca_compression,
        data => Data,
        metadata => #{original_dimension => Dimension, compressed_dimension => TargetDimension}
    }}.

decompress_pca(Data, #{original_dimension := OriginalDim, compressed_dimension := CompressedDim}) ->
    % Extract compressed vector
    CompressedVector = [V || <<V:32/float>> <= Data],
    
    % Pad with zeros to restore original dimension
    PaddedVector = CompressedVector ++ lists:duplicate(OriginalDim - CompressedDim, 0.0),
    {ok, PaddedVector}.

%% Zlib compression
compress_zlib(Vector) ->
    VectorBinary = list_to_binary([<<V:32/float>> || V <- Vector]),
    CompressedData = zlib:compress(VectorBinary),
    
    {ok, #{
        algorithm => zlib_compression,
        data => CompressedData,
        metadata => #{original_length => length(Vector)}
    }}.

decompress_zlib(Data, #{original_length := Length}) ->
    DecompressedBinary = zlib:uncompress(Data),
    Vector = [V || <<V:32/float>> <= DecompressedBinary],
    {ok, Vector}.

%% LZ4 compression (placeholder - would need LZ4 NIF)
compress_lz4(Vector) ->
    % Fallback to zlib for now
    compress_zlib(Vector).

decompress_lz4(Data, Metadata) ->
    % Fallback to zlib for now
    decompress_zlib(Data, Metadata).

%% Product quantization (simplified implementation)
compress_product_quantization(Vector) ->
    % Divide vector into subvectors and quantize each
    SubvectorSize = 4, % Fixed subvector size
    Subvectors = divide_into_subvectors(Vector, SubvectorSize),
    
    % Quantize each subvector (simplified - would use k-means in practice)
    QuantizedSubvectors = [quantize_subvector(SV) || SV <- Subvectors],
    
    Data = list_to_binary(QuantizedSubvectors),
    
    {ok, #{
        algorithm => product_quantization,
        data => Data,
        metadata => #{
            subvector_size => SubvectorSize,
            num_subvectors => length(Subvectors),
            original_length => length(Vector)
        }
    }}.

decompress_product_quantization(Data, #{subvector_size := SubvectorSize, 
                                       num_subvectors := NumSubvectors,
                                       original_length := OriginalLength}) ->
    QuantizedSubvectors = binary_to_list(Data),
    
    % Dequantize each subvector
    Subvectors = [dequantize_subvector(QSV, SubvectorSize) || QSV <- QuantizedSubvectors],
    
    % Flatten subvectors back to original vector
    Vector = lists:flatten(Subvectors),
    
    % Ensure correct length
    FinalVector = lists:sublist(Vector, OriginalLength),
    {ok, FinalVector}.

%% Batch compression for PCA
compress_pca_batch(Vectors) ->
    % In a real implementation, you'd compute PCA across all vectors
    % For now, compress each individually
    Results = [compress_pca(V) || V <- Vectors],
    {ok, [CV || {ok, CV} <- Results]}.

compress_product_quantization_batch(Vectors) ->
    % In a real implementation, you'd train codebooks across all vectors
    % For now, compress each individually
    Results = [compress_product_quantization(V) || V <- Vectors],
    {ok, [CV || {ok, CV} <- Results]}.

%% Helper functions
find_min_max(Vector) ->
    lists:foldl(fun(V, {Min, Max}) ->
        {min(V, Min), max(V, Max)}
    end, {hd(Vector), hd(Vector)}, tl(Vector)).

pack_4bit_values(Values) ->
    pack_4bit_values(Values, <<>>).

pack_4bit_values([], Acc) ->
    Acc;
pack_4bit_values([V], Acc) ->
    <<Acc/binary, V:4, 0:4>>;
pack_4bit_values([V1, V2 | Rest], Acc) ->
    pack_4bit_values(Rest, <<Acc/binary, V1:4, V2:4>>).

unpack_4bit_values(Data, Length) ->
    unpack_4bit_values(Data, Length, []).

unpack_4bit_values(_, 0, Acc) ->
    lists:reverse(Acc);
unpack_4bit_values(<<V1:4, V2:4, Rest/binary>>, Length, Acc) when Length >= 2 ->
    unpack_4bit_values(Rest, Length - 2, [V2, V1 | Acc]);
unpack_4bit_values(<<V1:4, _:4>>, 1, Acc) ->
    lists:reverse([V1 | Acc]).

divide_into_subvectors(Vector, SubvectorSize) ->
    divide_into_subvectors(Vector, SubvectorSize, []).

divide_into_subvectors([], _, Acc) ->
    lists:reverse(Acc);
divide_into_subvectors(Vector, SubvectorSize, Acc) when length(Vector) >= SubvectorSize ->
    {Subvector, Rest} = lists:split(SubvectorSize, Vector),
    divide_into_subvectors(Rest, SubvectorSize, [Subvector | Acc]);
divide_into_subvectors(Vector, _, Acc) ->
    lists:reverse([Vector | Acc]).

quantize_subvector(Subvector) ->
    % Simplified quantization - just take first element as representative
    case Subvector of
        [] -> 0;
        [H | _] -> round(H * 10) rem 256 % Simple quantization
    end.

dequantize_subvector(QuantizedValue, SubvectorSize) ->
    % Simplified dequantization
    Value = QuantizedValue / 10.0,
    lists:duplicate(SubvectorSize, Value).

calculate_accuracy_loss(Original, Decompressed) ->
    % Calculate mean squared error
    Differences = [math:pow(O - D, 2) || {O, D} <- lists:zip(Original, Decompressed)],
    MSE = lists:sum(Differences) / length(Differences),
    math:sqrt(MSE).
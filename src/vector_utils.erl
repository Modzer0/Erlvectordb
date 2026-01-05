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

-module(vector_utils).

-export([
    cosine_similarity/2,
    euclidean_distance/2,
    manhattan_distance/2,
    dot_product/2,
    normalize/1,
    vector_norm/1,
    vector_add/2,
    vector_subtract/2,
    vector_multiply/2
]).

%% Vector similarity and distance functions
cosine_similarity(V1, V2) ->
    DotProd = dot_product(V1, V2),
    Norm1 = vector_norm(V1),
    Norm2 = vector_norm(V2),
    case {Norm1, Norm2} of
        {0.0, _} -> 0.0;
        {_, 0.0} -> 0.0;
        _ -> DotProd / (Norm1 * Norm2)
    end.

euclidean_distance(V1, V2) ->
    Diff = vector_subtract(V1, V2),
    vector_norm(Diff).

manhattan_distance(V1, V2) ->
    lists:sum([abs(X - Y) || {X, Y} <- lists:zip(V1, V2)]).

%% Basic vector operations
dot_product(V1, V2) ->
    lists:sum([X * Y || {X, Y} <- lists:zip(V1, V2)]).

normalize(Vector) ->
    Norm = vector_norm(Vector),
    case Norm of
        0.0 -> Vector;
        _ -> [X / Norm || X <- Vector]
    end.

vector_norm(Vector) ->
    math:sqrt(lists:sum([X * X || X <- Vector])).

vector_add(V1, V2) ->
    [X + Y || {X, Y} <- lists:zip(V1, V2)].

vector_subtract(V1, V2) ->
    [X - Y || {X, Y} <- lists:zip(V1, V2)].

vector_multiply(Vector, Scalar) ->
    [X * Scalar || X <- Vector].

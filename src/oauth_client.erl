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

-module(oauth_client).

-export([
    get_access_token/3,
    refresh_access_token/1,
    make_authenticated_request/3,
    register_client/3
]).

-record(token_response, {
    access_token :: binary(),
    token_type :: binary(),
    expires_in :: integer(),
    refresh_token :: binary() | undefined,
    scope :: binary() | undefined
}).

%% Get access token using client credentials flow
get_access_token(ClientId, ClientSecret, Scopes) ->
    TokenEndpoint = application:get_env(erlvectordb, oauth_token_endpoint, "http://localhost:8081/oauth/token"),
    
    Body = uri_string:compose_query([
        {"grant_type", "client_credentials"},
        {"client_id", binary_to_list(ClientId)},
        {"client_secret", binary_to_list(ClientSecret)},
        {"scope", binary_to_list(iolist_to_binary(lists:join(" ", Scopes)))}
    ]),
    
    Headers = [
        {"Content-Type", "application/x-www-form-urlencoded"},
        {"Accept", "application/json"}
    ],
    
    case httpc:request(post, {TokenEndpoint, Headers, "application/x-www-form-urlencoded", Body}, [], []) of
        {ok, {{_, 200, _}, _ResponseHeaders, ResponseBody}} ->
            try
                TokenData = jsx:decode(list_to_binary(ResponseBody), [return_maps]),
                TokenResponse = #token_response{
                    access_token = maps:get(<<"access_token">>, TokenData),
                    token_type = maps:get(<<"token_type">>, TokenData),
                    expires_in = maps:get(<<"expires_in">>, TokenData),
                    refresh_token = maps:get(<<"refresh_token">>, TokenData, undefined),
                    scope = maps:get(<<"scope">>, TokenData, undefined)
                },
                {ok, TokenResponse}
            catch
                _:Error ->
                    {error, {parse_error, Error}}
            end;
        {ok, {{_, StatusCode, _}, _ResponseHeaders, ResponseBody}} ->
            try
                ErrorData = jsx:decode(list_to_binary(ResponseBody), [return_maps]),
                Error = maps:get(<<"error">>, ErrorData, <<"unknown_error">>),
                Description = maps:get(<<"error_description">>, ErrorData, <<"Unknown error">>),
                {error, {oauth_error, StatusCode, Error, Description}}
            catch
                _:_ ->
                    {error, {http_error, StatusCode, ResponseBody}}
            end;
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

%% Refresh access token
refresh_access_token(RefreshToken) ->
    TokenEndpoint = application:get_env(erlvectordb, oauth_token_endpoint, "http://localhost:8081/oauth/token"),
    
    Body = uri_string:compose_query([
        {"grant_type", "refresh_token"},
        {"refresh_token", binary_to_list(RefreshToken)}
    ]),
    
    Headers = [
        {"Content-Type", "application/x-www-form-urlencoded"},
        {"Accept", "application/json"}
    ],
    
    case httpc:request(post, {TokenEndpoint, Headers, "application/x-www-form-urlencoded", Body}, [], []) of
        {ok, {{_, 200, _}, _ResponseHeaders, ResponseBody}} ->
            try
                TokenData = jsx:decode(list_to_binary(ResponseBody), [return_maps]),
                TokenResponse = #token_response{
                    access_token = maps:get(<<"access_token">>, TokenData),
                    token_type = maps:get(<<"token_type">>, TokenData),
                    expires_in = maps:get(<<"expires_in">>, TokenData),
                    refresh_token = maps:get(<<"refresh_token">>, TokenData, RefreshToken),
                    scope = maps:get(<<"scope">>, TokenData, undefined)
                },
                {ok, TokenResponse}
            catch
                _:Error ->
                    {error, {parse_error, Error}}
            end;
        {ok, {{_, StatusCode, _}, _ResponseHeaders, ResponseBody}} ->
            try
                ErrorData = jsx:decode(list_to_binary(ResponseBody), [return_maps]),
                Error = maps:get(<<"error">>, ErrorData, <<"unknown_error">>),
                Description = maps:get(<<"error_description">>, ErrorData, <<"Unknown error">>),
                {error, {oauth_error, StatusCode, Error, Description}}
            catch
                _:_ ->
                    {error, {http_error, StatusCode, ResponseBody}}
            end;
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

%% Make authenticated MCP request
make_authenticated_request(AccessToken, MCPRequest, MCPEndpoint) ->
    % Add authentication to MCP request
    AuthenticatedRequest = MCPRequest#{
        <<"auth">> => #{
            <<"type">> => <<"bearer">>,
            <<"token">> => AccessToken
        }
    },
    
    RequestBody = jsx:encode(AuthenticatedRequest),
    
    case gen_tcp:connect("localhost", 8080, [binary, {packet, 0}, {active, false}]) of
        {ok, Socket} ->
            case gen_tcp:send(Socket, RequestBody) of
                ok ->
                    case gen_tcp:recv(Socket, 0) of
                        {ok, ResponseData} ->
                            gen_tcp:close(Socket),
                            try
                                Response = jsx:decode(ResponseData, [return_maps]),
                                {ok, Response}
                            catch
                                _:Error ->
                                    {error, {parse_error, Error}}
                            end;
                        {error, Reason} ->
                            gen_tcp:close(Socket),
                            {error, {recv_error, Reason}}
                    end;
                {error, Reason} ->
                    gen_tcp:close(Socket),
                    {error, {send_error, Reason}}
            end;
        {error, Reason} ->
            {error, {connection_error, Reason}}
    end.

%% Register a new OAuth client
register_client(ClientId, ClientSecret, Options) ->
    oauth_server:register_client(ClientId, ClientSecret, Options).

%% Example usage functions
-ifdef(TEST).
example_usage() ->
    % Register a client
    {ok, _} = register_client(<<"my_client">>, <<"my_secret">>, #{
        scopes => [<<"read">>, <<"write">>],
        grant_types => [<<"client_credentials">>, <<"refresh_token">>]
    }),
    
    % Get access token
    {ok, TokenResponse} = get_access_token(<<"my_client">>, <<"my_secret">>, [<<"read">>, <<"write">>]),
    AccessToken = TokenResponse#token_response.access_token,
    
    % Make authenticated MCP request
    MCPRequest = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"tools/list">>,
        <<"id">> => 1
    },
    
    {ok, Response} = make_authenticated_request(AccessToken, MCPRequest, "localhost:8080"),
    Response.
-endif.
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

-module(oauth_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() ->
    [
        test_client_registration,
        test_client_authentication,
        test_token_generation,
        test_token_validation,
        test_token_refresh,
        test_mcp_authentication,
        test_scope_permissions
    ].

init_per_suite(Config) ->
    application:set_env(erlvectordb, oauth_enabled, true),
    application:set_env(erlvectordb, create_default_client, false),
    application:ensure_all_started(erlvectordb),
    timer:sleep(1000), % Wait for servers to start
    Config.

end_per_suite(_Config) ->
    application:stop(erlvectordb),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

test_client_registration(_Config) ->
    ClientId = <<"test_client">>,
    ClientSecret = <<"test_secret">>,
    Options = #{
        scopes => [<<"read">>, <<"write">>],
        grant_types => [<<"client_credentials">>]
    },
    
    {ok, ClientId} = oauth_server:register_client(ClientId, ClientSecret, Options),
    
    % Try to register same client again (should fail)
    {error, client_already_exists} = oauth_server:register_client(ClientId, ClientSecret, Options).

test_client_authentication(_Config) ->
    ClientId = <<"auth_test_client">>,
    ClientSecret = <<"auth_test_secret">>,
    Options = #{scopes => [<<"read">>]},
    
    {ok, _} = oauth_server:register_client(ClientId, ClientSecret, Options),
    
    % Test valid authentication
    {ok, authenticated} = oauth_server:authenticate_client(ClientId, ClientSecret),
    
    % Test invalid credentials
    {error, invalid_credentials} = oauth_server:authenticate_client(ClientId, <<"wrong_secret">>),
    
    % Test non-existent client
    {error, invalid_client} = oauth_server:authenticate_client(<<"nonexistent">>, <<"secret">>).

test_token_generation(_Config) ->
    ClientId = <<"token_test_client">>,
    ClientSecret = <<"token_test_secret">>,
    Options = #{scopes => [<<"read">>, <<"write">>]},
    
    {ok, _} = oauth_server:register_client(ClientId, ClientSecret, Options),
    
    % Generate access token
    {ok, TokenResponse} = oauth_server:generate_access_token(ClientId, [<<"read">>]),
    
    AccessToken = maps:get(access_token, TokenResponse),
    TokenType = maps:get(token_type, TokenResponse),
    ExpiresIn = maps:get(expires_in, TokenResponse),
    
    true = is_binary(AccessToken),
    <<"Bearer">> = TokenType,
    true = is_integer(ExpiresIn),
    true = ExpiresIn > 0.

test_token_validation(_Config) ->
    ClientId = <<"validation_test_client">>,
    ClientSecret = <<"validation_test_secret">>,
    Options = #{scopes => [<<"read">>, <<"write">>]},
    
    {ok, _} = oauth_server:register_client(ClientId, ClientSecret, Options),
    {ok, TokenResponse} = oauth_server:generate_access_token(ClientId, [<<"read">>, <<"write">>]),
    
    AccessToken = maps:get(access_token, TokenResponse),
    
    % Validate token
    {ok, TokenInfo} = oauth_server:validate_token(AccessToken),
    ClientId = maps:get(client_id, TokenInfo),
    [<<"read">>, <<"write">>] = maps:get(scopes, TokenInfo),
    
    % Test invalid token
    {error, invalid_token} = oauth_server:validate_token(<<"invalid_token">>).

test_token_refresh(_Config) ->
    ClientId = <<"refresh_test_client">>,
    ClientSecret = <<"refresh_test_secret">>,
    Options = #{
        scopes => [<<"read">>, <<"write">>],
        grant_types => [<<"client_credentials">>, <<"refresh_token">>]
    },
    
    {ok, _} = oauth_server:register_client(ClientId, ClientSecret, Options),
    {ok, TokenResponse} = oauth_server:generate_access_token(ClientId, [<<"read">>]),
    
    RefreshToken = maps:get(refresh_token, TokenResponse),
    
    % Refresh token
    {ok, NewTokenResponse} = oauth_server:refresh_token(RefreshToken),
    
    NewAccessToken = maps:get(access_token, NewTokenResponse),
    NewRefreshToken = maps:get(refresh_token, NewTokenResponse),
    
    true = is_binary(NewAccessToken),
    true = is_binary(NewRefreshToken),
    true = NewAccessToken =/= maps:get(access_token, TokenResponse),
    
    % Old refresh token should be invalid now
    {error, invalid_refresh_token} = oauth_server:refresh_token(RefreshToken).

test_mcp_authentication(_Config) ->
    ClientId = <<"mcp_test_client">>,
    ClientSecret = <<"mcp_test_secret">>,
    Options = #{scopes => [<<"read">>, <<"write">>]},
    
    {ok, _} = oauth_server:register_client(ClientId, ClientSecret, Options),
    {ok, TokenResponse} = oauth_server:generate_access_token(ClientId, [<<"read">>]),
    
    AccessToken = maps:get(access_token, TokenResponse),
    
    % Test MCP request with valid token
    MCPRequest = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"tools/list">>,
        <<"id">> => 1,
        <<"auth">> => #{
            <<"type">> => <<"bearer">>,
            <<"token">> => AccessToken
        }
    },
    
    {ok, ClientInfo} = mcp_server:authenticate_mcp_request(MCPRequest),
    ClientId = maps:get(client_id, ClientInfo),
    [<<"read">>] = maps:get(scopes, ClientInfo),
    
    % Test MCP request without token
    MCPRequestNoAuth = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"tools/list">>,
        <<"id">> => 1
    },
    
    {error, auth_error} = mcp_server:authenticate_mcp_request(MCPRequestNoAuth).

test_scope_permissions(_Config) ->
    % Test read-only client
    ReadClientId = <<"read_only_client">>,
    {ok, _} = oauth_server:register_client(ReadClientId, <<"secret">>, #{scopes => [<<"read">>]}),
    
    % Test write client
    WriteClientId = <<"write_client">>,
    {ok, _} = oauth_server:register_client(WriteClientId, <<"secret">>, #{scopes => [<<"write">>]}),
    
    % Test admin client
    AdminClientId = <<"admin_client">>,
    {ok, _} = oauth_server:register_client(AdminClientId, <<"secret">>, #{scopes => [<<"admin">>]}),
    
    % Check tool permissions
    true = mcp_server:check_tool_permission(<<"search_vectors">>, [<<"read">>]),
    false = mcp_server:check_tool_permission(<<"insert_vector">>, [<<"read">>]),
    true = mcp_server:check_tool_permission(<<"insert_vector">>, [<<"write">>]),
    false = mcp_server:check_tool_permission(<<"backup_store">>, [<<"write">>]),
    true = mcp_server:check_tool_permission(<<"backup_store">>, [<<"admin">>]).
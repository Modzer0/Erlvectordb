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

-module(oauth_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([
    register_client/3,
    authenticate_client/2,
    generate_access_token/2,
    validate_token/1,
    revoke_token/1,
    refresh_token/1,
    get_client_info/1
]).

-record(state, {
    clients = #{} :: map(),
    tokens = #{} :: map(),
    refresh_tokens = #{} :: map()
}).

-record(oauth_client, {
    client_id :: binary(),
    client_secret :: binary(),
    redirect_uris = [] :: [binary()],
    scopes = [] :: [binary()],
    grant_types = [<<"client_credentials">>] :: [binary()],
    created_at :: integer(),
    active = true :: boolean()
}).

-record(access_token, {
    token :: binary(),
    client_id :: binary(),
    scopes = [] :: [binary()],
    expires_at :: integer(),
    created_at :: integer()
}).

-record(refresh_token, {
    token :: binary(),
    access_token :: binary(),
    client_id :: binary(),
    expires_at :: integer(),
    created_at :: integer()
}).

%% API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register_client(ClientId, ClientSecret, Options) ->
    gen_server:call(?MODULE, {register_client, ClientId, ClientSecret, Options}).

authenticate_client(ClientId, ClientSecret) ->
    gen_server:call(?MODULE, {authenticate_client, ClientId, ClientSecret}).

generate_access_token(ClientId, Scopes) ->
    gen_server:call(?MODULE, {generate_access_token, ClientId, Scopes}).

validate_token(Token) ->
    gen_server:call(?MODULE, {validate_token, Token}).

revoke_token(Token) ->
    gen_server:call(?MODULE, {revoke_token, Token}).

refresh_token(RefreshToken) ->
    gen_server:call(?MODULE, {refresh_token, RefreshToken}).

get_client_info(ClientId) ->
    gen_server:call(?MODULE, {get_client_info, ClientId}).

%% Callbacks
init([]) ->
    % Create default admin client if configured
    State = case application:get_env(erlvectordb, create_default_client, false) of
        true ->
            DefaultClientId = application:get_env(erlvectordb, default_client_id, <<"admin">>),
            DefaultClientSecret = application:get_env(erlvectordb, default_client_secret, generate_secret()),
            
            Client = #oauth_client{
                client_id = DefaultClientId,
                client_secret = hash_secret(DefaultClientSecret),
                scopes = [<<"read">>, <<"write">>, <<"admin">>],
                grant_types = [<<"client_credentials">>, <<"refresh_token">>],
                created_at = erlang:system_time(millisecond)
            },
            
            error_logger:info_msg("Default OAuth client created - ID: ~s, Secret: ~s~n", 
                                [DefaultClientId, DefaultClientSecret]),
            
            #state{clients = #{DefaultClientId => Client}};
        false ->
            #state{}
    end,
    
    % Start token cleanup timer
    timer:send_interval(300000, cleanup_expired_tokens), % 5 minutes
    
    {ok, State}.

handle_call({register_client, ClientId, ClientSecret, Options}, _From, State) ->
    case maps:get(ClientId, State#state.clients, undefined) of
        undefined ->
            RedirectUris = maps:get(redirect_uris, Options, []),
            Scopes = maps:get(scopes, Options, [<<"read">>, <<"write">>]),
            GrantTypes = maps:get(grant_types, Options, [<<"client_credentials">>]),
            
            Client = #oauth_client{
                client_id = ClientId,
                client_secret = hash_secret(ClientSecret),
                redirect_uris = RedirectUris,
                scopes = Scopes,
                grant_types = GrantTypes,
                created_at = erlang:system_time(millisecond)
            },
            
            NewClients = maps:put(ClientId, Client, State#state.clients),
            {reply, {ok, ClientId}, State#state{clients = NewClients}};
        _ ->
            {reply, {error, client_already_exists}, State}
    end;

handle_call({authenticate_client, ClientId, ClientSecret}, _From, State) ->
    case maps:get(ClientId, State#state.clients, undefined) of
        undefined ->
            {reply, {error, invalid_client}, State};
        #oauth_client{client_secret = HashedSecret, active = true} ->
            case verify_secret(ClientSecret, HashedSecret) of
                true -> {reply, {ok, authenticated}, State};
                false -> {reply, {error, invalid_credentials}, State}
            end;
        #oauth_client{active = false} ->
            {reply, {error, client_inactive}, State}
    end;

handle_call({generate_access_token, ClientId, Scopes}, _From, State) ->
    case maps:get(ClientId, State#state.clients, undefined) of
        undefined ->
            {reply, {error, invalid_client}, State};
        #oauth_client{scopes = AllowedScopes, active = true} ->
            % Validate requested scopes
            case validate_scopes(Scopes, AllowedScopes) of
                true ->
                    Token = generate_token(),
                    RefreshToken = generate_token(),
                    ExpiresAt = erlang:system_time(millisecond) + 
                               application:get_env(erlvectordb, token_lifetime, 3600000), % 1 hour
                    RefreshExpiresAt = erlang:system_time(millisecond) + 
                                      application:get_env(erlvectordb, refresh_token_lifetime, 86400000), % 24 hours
                    
                    AccessToken = #access_token{
                        token = Token,
                        client_id = ClientId,
                        scopes = Scopes,
                        expires_at = ExpiresAt,
                        created_at = erlang:system_time(millisecond)
                    },
                    
                    RefreshTokenRecord = #refresh_token{
                        token = RefreshToken,
                        access_token = Token,
                        client_id = ClientId,
                        expires_at = RefreshExpiresAt,
                        created_at = erlang:system_time(millisecond)
                    },
                    
                    NewTokens = maps:put(Token, AccessToken, State#state.tokens),
                    NewRefreshTokens = maps:put(RefreshToken, RefreshTokenRecord, State#state.refresh_tokens),
                    
                    Response = #{
                        access_token => Token,
                        token_type => <<"Bearer">>,
                        expires_in => application:get_env(erlvectordb, token_lifetime, 3600000) div 1000,
                        refresh_token => RefreshToken,
                        scope => binary:list_to_bin(lists:join(<<" ">>, Scopes))
                    },
                    
                    {reply, {ok, Response}, State#state{
                        tokens = NewTokens,
                        refresh_tokens = NewRefreshTokens
                    }};
                false ->
                    {reply, {error, invalid_scope}, State}
            end;
        _ ->
            {reply, {error, client_inactive}, State}
    end;

handle_call({validate_token, Token}, _From, State) ->
    case maps:get(Token, State#state.tokens, undefined) of
        undefined ->
            {reply, {error, invalid_token}, State};
        #access_token{expires_at = ExpiresAt, client_id = ClientId, scopes = Scopes} ->
            case erlang:system_time(millisecond) < ExpiresAt of
                true ->
                    {reply, {ok, #{client_id => ClientId, scopes => Scopes}}, State};
                false ->
                    % Token expired, remove it
                    NewTokens = maps:remove(Token, State#state.tokens),
                    {reply, {error, token_expired}, State#state{tokens = NewTokens}}
            end
    end;

handle_call({revoke_token, Token}, _From, State) ->
    NewTokens = maps:remove(Token, State#state.tokens),
    % Also remove associated refresh tokens
    NewRefreshTokens = maps:filter(fun(_, #refresh_token{access_token = AT}) ->
        AT =/= Token
    end, State#state.refresh_tokens),
    
    {reply, ok, State#state{tokens = NewTokens, refresh_tokens = NewRefreshTokens}};

handle_call({refresh_token, RefreshToken}, _From, State) ->
    case maps:get(RefreshToken, State#state.refresh_tokens, undefined) of
        undefined ->
            {reply, {error, invalid_refresh_token}, State};
        #refresh_token{expires_at = ExpiresAt, client_id = ClientId, access_token = OldToken} ->
            case erlang:system_time(millisecond) < ExpiresAt of
                true ->
                    % Get old token scopes
                    Scopes = case maps:get(OldToken, State#state.tokens, undefined) of
                        #access_token{scopes = S} -> S;
                        _ -> [<<"read">>] % Default scope
                    end,
                    
                    % Generate new tokens
                    NewToken = generate_token(),
                    NewRefreshToken = generate_token(),
                    NewExpiresAt = erlang:system_time(millisecond) + 
                                  application:get_env(erlvectordb, token_lifetime, 3600000),
                    NewRefreshExpiresAt = erlang:system_time(millisecond) + 
                                         application:get_env(erlvectordb, refresh_token_lifetime, 86400000),
                    
                    AccessToken = #access_token{
                        token = NewToken,
                        client_id = ClientId,
                        scopes = Scopes,
                        expires_at = NewExpiresAt,
                        created_at = erlang:system_time(millisecond)
                    },
                    
                    RefreshTokenRecord = #refresh_token{
                        token = NewRefreshToken,
                        access_token = NewToken,
                        client_id = ClientId,
                        expires_at = NewRefreshExpiresAt,
                        created_at = erlang:system_time(millisecond)
                    },
                    
                    % Remove old tokens
                    CleanTokens = maps:remove(OldToken, State#state.tokens),
                    CleanRefreshTokens = maps:remove(RefreshToken, State#state.refresh_tokens),
                    
                    % Add new tokens
                    NewTokens = maps:put(NewToken, AccessToken, CleanTokens),
                    NewRefreshTokensMap = maps:put(NewRefreshToken, RefreshTokenRecord, CleanRefreshTokens),
                    
                    Response = #{
                        access_token => NewToken,
                        token_type => <<"Bearer">>,
                        expires_in => application:get_env(erlvectordb, token_lifetime, 3600000) div 1000,
                        refresh_token => NewRefreshToken,
                        scope => binary:list_to_bin(lists:join(<<" ">>, Scopes))
                    },
                    
                    {reply, {ok, Response}, State#state{
                        tokens = NewTokens,
                        refresh_tokens = NewRefreshTokensMap
                    }};
                false ->
                    % Refresh token expired
                    NewRefreshTokens = maps:remove(RefreshToken, State#state.refresh_tokens),
                    {reply, {error, refresh_token_expired}, State#state{refresh_tokens = NewRefreshTokens}}
            end
    end;

handle_call({get_client_info, ClientId}, _From, State) ->
    case maps:get(ClientId, State#state.clients, undefined) of
        undefined ->
            {reply, {error, client_not_found}, State};
        #oauth_client{} = Client ->
            Info = #{
                client_id => Client#oauth_client.client_id,
                scopes => Client#oauth_client.scopes,
                grant_types => Client#oauth_client.grant_types,
                redirect_uris => Client#oauth_client.redirect_uris,
                created_at => Client#oauth_client.created_at,
                active => Client#oauth_client.active
            },
            {reply, {ok, Info}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup_expired_tokens, State) ->
    Now = erlang:system_time(millisecond),
    
    % Clean expired access tokens
    NewTokens = maps:filter(fun(_, #access_token{expires_at = ExpiresAt}) ->
        ExpiresAt > Now
    end, State#state.tokens),
    
    % Clean expired refresh tokens
    NewRefreshTokens = maps:filter(fun(_, #refresh_token{expires_at = ExpiresAt}) ->
        ExpiresAt > Now
    end, State#state.refresh_tokens),
    
    {noreply, State#state{tokens = NewTokens, refresh_tokens = NewRefreshTokens}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
generate_token() ->
    base64:encode(crypto:strong_rand_bytes(32)).

generate_secret() ->
    base64:encode(crypto:strong_rand_bytes(24)).

hash_secret(Secret) ->
    crypto:hash(sha256, Secret).

verify_secret(Secret, HashedSecret) ->
    crypto:hash(sha256, Secret) =:= HashedSecret.

validate_scopes(RequestedScopes, AllowedScopes) ->
    lists:all(fun(Scope) -> lists:member(Scope, AllowedScopes) end, RequestedScopes).
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

-module(oauth_http_handler).

-export([start_link/0, handle_request/3]).

start_link() ->
    % Get port from port manager instead of direct configuration
    case port_manager:get_service_port(oauth_server) of
        {ok, Port} ->
            {ok, spawn_link(fun() -> start_server(Port) end)};
        {error, service_not_allocated} ->
            error_logger:error_msg("OAuth server port not allocated by port manager~n"),
            {error, port_not_allocated};
        {error, {service_not_bound, Status}} ->
            error_logger:error_msg("OAuth server port not bound by port manager, status: ~p~n", [Status]),
            {error, {port_not_bound, Status}};
        {error, Reason} ->
            error_logger:error_msg("OAuth server failed to get port from port manager: ~p~n", [Reason]),
            {error, {port_manager_error, Reason}}
    end.

start_server(Port) ->
    case gen_tcp:listen(Port, [binary, {packet, http_bin}, {active, false}, {reuseaddr, true}]) of
        {ok, ListenSocket} ->
            error_logger:info_msg("OAuth server listening on port ~p~n", [Port]),
            accept_loop(ListenSocket);
        {error, Reason} ->
            error_logger:error_msg("Failed to start OAuth server: ~p~n", [Reason]),
            exit({oauth_server_failed, Reason})
    end.

accept_loop(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            spawn(fun() -> handle_client(Socket) end),
            accept_loop(ListenSocket);
        {error, closed} ->
            ok;
        {error, Reason} ->
            error_logger:error_msg("OAuth accept error: ~p~n", [Reason]),
            accept_loop(ListenSocket)
    end.

handle_client(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, {http_request, Method, {abs_path, Path}, _Version}} ->
            Headers = recv_headers(Socket, []),
            Body = recv_body(Socket, Headers),
            
            Response = handle_request(Method, Path, #{headers => Headers, body => Body}),
            send_response(Socket, Response);
        {error, {http_error, _}} ->
            send_error_response(Socket, 400, <<"Bad Request">>);
        {error, closed} ->
            ok;
        {error, Reason} ->
            error_logger:error_msg("OAuth client error: ~p~n", [Reason])
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
    % Switch to raw mode to read body
    inet:setopts(Socket, [{packet, raw}]),
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

handle_request('POST', <<"/oauth/token">>, #{body := Body}) ->
    try
        Params = parse_form_data(Body),
        handle_token_request(Params)
    catch
        _:Error ->
            error_response(400, <<"invalid_request">>, io_lib:format("~p", [Error]))
    end;

handle_request('POST', <<"/oauth/revoke">>, #{headers := Headers, body := Body}) ->
    case authenticate_request(Headers) of
        {ok, _ClientId} ->
            try
                Params = parse_form_data(Body),
                Token = maps:get(<<"token">>, Params),
                oauth_server:revoke_token(Token),
                success_response(#{<<"revoked">> => true})
            catch
                _:Error ->
                    error_response(400, <<"invalid_request">>, io_lib:format("~p", [Error]))
            end;
        {error, Reason} ->
            auth_error_response(Reason)
    end;

handle_request('GET', <<"/oauth/client_info">>, #{headers := Headers}) ->
    case authenticate_request(Headers) of
        {ok, ClientId} ->
            case oauth_server:get_client_info(ClientId) of
                {ok, Info} -> success_response(Info);
                {error, Reason} -> error_response(404, <<"client_not_found">>, Reason)
            end;
        {error, Reason} ->
            auth_error_response(Reason)
    end;

handle_request('OPTIONS', _, _) ->
    cors_response();

handle_request(_, _, _) ->
    error_response(404, <<"not_found">>, <<"Endpoint not found">>).

handle_token_request(Params) ->
    GrantType = maps:get(<<"grant_type">>, Params),
    case GrantType of
        <<"client_credentials">> ->
            handle_client_credentials(Params);
        <<"refresh_token">> ->
            handle_refresh_token(Params);
        _ ->
            error_response(400, <<"unsupported_grant_type">>, <<"Grant type not supported">>)
    end.

handle_client_credentials(Params) ->
    ClientId = maps:get(<<"client_id">>, Params),
    ClientSecret = maps:get(<<"client_secret">>, Params),
    Scope = maps:get(<<"scope">>, Params, <<"read write">>),
    
    case oauth_server:authenticate_client(ClientId, ClientSecret) of
        {ok, authenticated} ->
            Scopes = binary:split(Scope, <<" ">>, [global]),
            case oauth_server:generate_access_token(ClientId, Scopes) of
                {ok, TokenResponse} ->
                    success_response(TokenResponse);
                {error, Reason} ->
                    error_response(400, <<"invalid_scope">>, Reason)
            end;
        {error, Reason} ->
            auth_error_response(Reason)
    end.

handle_refresh_token(Params) ->
    RefreshToken = maps:get(<<"refresh_token">>, Params),
    case oauth_server:refresh_token(RefreshToken) of
        {ok, TokenResponse} ->
            success_response(TokenResponse);
        {error, invalid_refresh_token} ->
            error_response(400, <<"invalid_grant">>, <<"Invalid refresh token">>);
        {error, refresh_token_expired} ->
            error_response(400, <<"invalid_grant">>, <<"Refresh token expired">>);
        {error, Reason} ->
            error_response(400, <<"invalid_grant">>, Reason)
    end.

authenticate_request(Headers) ->
    case lists:keyfind('Authorization', 1, Headers) of
        {'Authorization', <<"Basic ", Encoded/binary>>} ->
            try
                Decoded = base64:decode(Encoded),
                [ClientId, ClientSecret] = binary:split(Decoded, <<":">>),
                case oauth_server:authenticate_client(ClientId, ClientSecret) of
                    {ok, authenticated} -> {ok, ClientId};
                    {error, Reason} -> {error, Reason}
                end
            catch
                _:_ -> {error, invalid_credentials}
            end;
        {'Authorization', <<"Bearer ", Token/binary>>} ->
            case oauth_server:validate_token(Token) of
                {ok, #{client_id := ClientId}} -> {ok, ClientId};
                {error, Reason} -> {error, Reason}
            end;
        _ ->
            {error, missing_authorization}
    end.

parse_form_data(Body) ->
    Pairs = binary:split(Body, <<"&">>, [global]),
    maps:from_list([begin
        case binary:split(Pair, <<"=">>) of
            [Key, Value] ->
                % Decode and convert back to binary
                DecodedKey = list_to_binary(uri_string:percent_decode(binary_to_list(Key))),
                DecodedValue = list_to_binary(uri_string:percent_decode(binary_to_list(Value))),
                {DecodedKey, DecodedValue};
            [Key] ->
                DecodedKey = list_to_binary(uri_string:percent_decode(binary_to_list(Key))),
                {DecodedKey, <<>>}
        end
    end || Pair <- Pairs, Pair =/= <<>>]).

success_response(Data) ->
    Body = jsx:encode(Data),
    {200, [
        {<<"Content-Type">>, <<"application/json">>},
        {<<"Cache-Control">>, <<"no-store">>},
        {<<"Pragma">>, <<"no-cache">>}
    ], Body}.

error_response(Status, Error, Description) ->
    Body = jsx:encode(#{
        <<"error">> => Error,
        <<"error_description">> => iolist_to_binary(Description)
    }),
    {Status, [
        {<<"Content-Type">>, <<"application/json">>},
        {<<"Cache-Control">>, <<"no-store">>},
        {<<"Pragma">>, <<"no-cache">>}
    ], Body}.

auth_error_response(invalid_client) ->
    error_response(401, <<"invalid_client">>, <<"Client authentication failed">>);
auth_error_response(invalid_credentials) ->
    error_response(401, <<"invalid_client">>, <<"Invalid client credentials">>);
auth_error_response(client_inactive) ->
    error_response(401, <<"invalid_client">>, <<"Client is inactive">>);
auth_error_response(missing_authorization) ->
    error_response(401, <<"invalid_client">>, <<"Missing authorization header">>);
auth_error_response(invalid_token) ->
    error_response(401, <<"invalid_token">>, <<"Invalid access token">>);
auth_error_response(token_expired) ->
    error_response(401, <<"invalid_token">>, <<"Access token expired">>);
auth_error_response(Reason) ->
    error_response(401, <<"invalid_client">>, Reason).

cors_response() ->
    {200, [
        {<<"Access-Control-Allow-Origin">>, <<"*">>},
        {<<"Access-Control-Allow-Methods">>, <<"GET, POST, OPTIONS">>},
        {<<"Access-Control-Allow-Headers">>, <<"Authorization, Content-Type">>},
        {<<"Access-Control-Max-Age">>, <<"86400">>}
    ], <<>>}.

send_response(Socket, {Status, Headers, Body}) ->
    StatusLine = io_lib:format("HTTP/1.1 ~w ~s\r\n", [Status, status_text(Status)]),
    HeaderLines = [[header_name_to_list(Name), ": ", Value, "\r\n"] || {Name, Value} <- Headers],
    Response = [StatusLine, HeaderLines, "\r\n", Body],
    gen_tcp:send(Socket, Response).

header_name_to_list(Name) when is_atom(Name) -> atom_to_list(Name);
header_name_to_list(Name) when is_binary(Name) -> binary_to_list(Name);
header_name_to_list(Name) when is_list(Name) -> Name.

send_error_response(Socket, Status, Message) ->
    send_response(Socket, {Status, [{<<"Content-Type">>, <<"text/plain">>}], Message}).

status_text(200) -> "OK";
status_text(400) -> "Bad Request";
status_text(401) -> "Unauthorized";
status_text(404) -> "Not Found";
status_text(500) -> "Internal Server Error";
status_text(_) -> "Unknown".
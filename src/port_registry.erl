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

-module(port_registry).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    register_port/2,
    release_port/1,
    is_port_available/1,
    find_available_port/1,
    get_all_allocations/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Types
-type service_name() :: atom().
-type port_binding() :: #{
    port => integer(),
    service => service_name(),
    pid => pid(),
    bind_time => integer()
}.

%% Records
-record(port_binding, {
    port :: integer(),
    service :: service_name(),
    pid :: pid() | undefined,
    bind_time :: integer()
}).

-record(state, {
    bindings :: #{integer() => #port_binding{}},
    reserved_ports :: [integer()]
}).

-define(SERVER, ?MODULE).
-define(MIN_PORT, 1024).
-define(MAX_PORT, 65535).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

register_port(Port, Service) when is_integer(Port), is_atom(Service) ->
    gen_server:call(?SERVER, {register_port, Port, Service}).

release_port(Port) when is_integer(Port) ->
    gen_server:call(?SERVER, {release_port, Port}).

is_port_available(Port) when is_integer(Port) ->
    gen_server:call(?SERVER, {is_port_available, Port}).

find_available_port({PreferredPort, {RangeStart, RangeEnd}}) ->
    gen_server:call(?SERVER, {find_available_port, PreferredPort, RangeStart, RangeEnd}).

get_all_allocations() ->
    gen_server:call(?SERVER, get_all_allocations).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    State = #state{
        bindings = #{},
        reserved_ports = get_reserved_ports()
    },
    {ok, State}.

handle_call({register_port, Port, Service}, _From, State) ->
    case validate_port(Port) of
        ok ->
            case maps:is_key(Port, State#state.bindings) of
                true ->
                    {reply, {error, port_already_registered}, State};
                false ->
                    Binding = #port_binding{
                        port = Port,
                        service = Service,
                        pid = self(),
                        bind_time = erlang:system_time(second)
                    },
                    NewBindings = maps:put(Port, Binding, State#state.bindings),
                    NewState = State#state{bindings = NewBindings},
                    {reply, ok, NewState}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({release_port, Port}, _From, State) ->
    case maps:get(Port, State#state.bindings, undefined) of
        undefined ->
            {reply, {error, port_not_registered}, State};
        _Binding ->
            NewBindings = maps:remove(Port, State#state.bindings),
            NewState = State#state{bindings = NewBindings},
            {reply, ok, NewState}
    end;

handle_call({is_port_available, Port}, _From, State) ->
    Available = not maps:is_key(Port, State#state.bindings) andalso
                not lists:member(Port, State#state.reserved_ports) andalso
                is_port_bindable(Port),
    {reply, Available, State};

handle_call({find_available_port, PreferredPort, RangeStart, RangeEnd}, _From, State) ->
    Reply = do_find_available_port(PreferredPort, RangeStart, RangeEnd, State),
    {reply, Reply, State};

handle_call(get_all_allocations, _From, State) ->
    Allocations = maps:fold(fun(Port, Binding, Acc) ->
        Info = #{
            port => Port,
            service => Binding#port_binding.service,
            pid => Binding#port_binding.pid,
            bind_time => Binding#port_binding.bind_time
        },
        [Info | Acc]
    end, [], State#state.bindings),
    {reply, {ok, Allocations}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

validate_port(Port) when is_integer(Port) ->
    if
        Port < ?MIN_PORT ->
            {error, {port_too_low, Port, ?MIN_PORT}};
        Port > ?MAX_PORT ->
            {error, {port_too_high, Port, ?MAX_PORT}};
        true ->
            ok
    end;
validate_port(Port) ->
    {error, {invalid_port_type, Port}}.

is_port_bindable(Port) ->
    case gen_tcp:listen(Port, [{reuseaddr, false}, {active, false}]) of
        {ok, Socket} ->
            gen_tcp:close(Socket),
            true;
        {error, eaddrinuse} ->
            false;
        {error, _} ->
            false
    end.

do_find_available_port(PreferredPort, RangeStart, RangeEnd, State) ->
    % Validate range
    case validate_port_range(RangeStart, RangeEnd) of
        ok ->
            % Try preferred port first if it's in range
            case PreferredPort >= RangeStart andalso PreferredPort =< RangeEnd of
                true ->
                    case is_port_available_internal(PreferredPort, State) of
                        true ->
                            {ok, PreferredPort};
                        false ->
                            find_next_available_port(RangeStart, RangeEnd, State, PreferredPort)
                    end;
                false ->
                    find_next_available_port(RangeStart, RangeEnd, State, RangeStart)
            end;
        {error, Reason} ->
            {error, Reason}
    end.

validate_port_range(Start, End) when is_integer(Start), is_integer(End) ->
    if
        Start < ?MIN_PORT ->
            {error, {range_start_too_low, Start, ?MIN_PORT}};
        End > ?MAX_PORT ->
            {error, {range_end_too_high, End, ?MAX_PORT}};
        Start > End ->
            {error, {invalid_range, Start, End}};
        true ->
            ok
    end;
validate_port_range(Start, End) ->
    {error, {invalid_range_types, Start, End}}.

find_next_available_port(RangeStart, RangeEnd, State, StartFrom) ->
    % Try ports from StartFrom to RangeEnd
    case find_in_range(StartFrom, RangeEnd, State) of
        {ok, Port} ->
            {ok, Port};
        not_found ->
            % If StartFrom was not RangeStart, try from RangeStart to StartFrom-1
            case StartFrom > RangeStart of
                true ->
                    find_in_range(RangeStart, StartFrom - 1, State);
                false ->
                    {error, no_ports_available_in_range}
            end
    end.

find_in_range(Start, End, State) when Start =< End ->
    case is_port_available_internal(Start, State) of
        true ->
            {ok, Start};
        false ->
            find_in_range(Start + 1, End, State)
    end;
find_in_range(_Start, _End, _State) ->
    not_found.

is_port_available_internal(Port, State) ->
    not maps:is_key(Port, State#state.bindings) andalso
    not lists:member(Port, State#state.reserved_ports) andalso
    is_port_bindable(Port).

get_reserved_ports() ->
    % Common reserved ports that should not be used
    [22, 23, 25, 53, 80, 110, 143, 443, 993, 995].
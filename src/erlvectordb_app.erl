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

-module(erlvectordb_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    case erlvectordb_sup:start_link() of
        {ok, Pid} ->
            % Check if startup coordination is disabled (for testing)
            case application:get_env(erlvectordb, disable_startup_coordination, false) of
                true ->
                    error_logger:info_msg("Startup coordination disabled~n");
                false ->
                    % After supervisor starts, coordinate the startup of network services
                    error_logger:info_msg("Application supervisor started, coordinating network service startup~n"),
                    
                    % Spawn a process to handle startup coordination to avoid blocking application start
                    spawn(fun() ->
                        % Give the supervisor a moment to fully initialize
                        timer:sleep(100),
                        
                        case startup_coordinator:coordinate_startup() of
                            {ok, startup_completed} ->
                                error_logger:info_msg("Network services startup coordination completed successfully~n");
                            {ok, already_completed} ->
                                error_logger:info_msg("Network services startup coordination already completed~n");
                            {error, Reason} ->
                                error_logger:error_msg("Network services startup coordination failed: ~p~n", [Reason]),
                                error_logger:error_msg("Application will continue running, but some network services may not be available~n")
                        end
                    end)
            end,
            
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.

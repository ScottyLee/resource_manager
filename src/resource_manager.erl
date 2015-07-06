%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc resource_manager.

-module(resource_manager).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the resource_manager server.
start() ->
    resource_manager_deps:ensure(),
    ensure_started(crypto),
    application:start(resource_manager).


%% @spec stop() -> ok
%% @doc Stop the resource_manager server.
stop() ->
    application:stop(resource_manager).

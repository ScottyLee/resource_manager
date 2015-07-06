%% @author Mochi Media <dev@mochimedia.com>
%% @copyright resource_manager Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the resource_manager application.

-module(resource_manager_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for resource_manager.
start(_Type, _StartArgs) ->
    resource_manager_deps:ensure(),
    resource_manager_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for resource_manager.
stop(_State) ->
    ok.

%%%-------------------------------------------------------------------
%% @doc project public API
%% @end
%%%-------------------------------------------------------------------

-module(project_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    project_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

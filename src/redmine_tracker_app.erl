%%%-------------------------------------------------------------------
%% @doc redmine_tracker public API
%% @end
%%%-------------------------------------------------------------------

-module(redmine_tracker_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    redmine_tracker_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

%%%-------------------------------------------------------------------
%% @doc redmine_tracker public API
%% @end
%%%-------------------------------------------------------------------

-module(redmine_tracker_app).

-behaviour(application).

-export([start/2, stop/1]).

%% TODO путь для схемы задаётся через параметр dir.
%% Пока что дефолт, но при установке пакета надо будет закрепить дефолтные пути в системе
%% ~/.local/state/redmine_tracker
start(_StartType, _StartArgs) ->
    logger:set_module_level(track, debug),
    ok = db:start(),
    ok = db:ensure_all_ready(),
    ok = db:init(),
    redmine_tracker_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

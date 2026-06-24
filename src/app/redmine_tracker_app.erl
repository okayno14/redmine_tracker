%%%-------------------------------------------------------------------
%% @doc redmine_tracker public API
%% @end
%%%-------------------------------------------------------------------

-module(redmine_tracker_app).

-behaviour(application).

-export([
    start/2,
    stop/1
]).

%% TODO путь для схемы задаётся через параметр dir.
%% Пока что дефолт, но при установке пакета надо будет закрепить дефолтные пути в системе
%% ~/.local/state/redmine_tracker
start(_StartType, _StartArgs) ->
    logger:set_module_level(track, debug),
    ok = set_path(),
    ok = db:start(),
    ok = db:ensure_all_ready(),
    ok = db:init(),
    redmine_tracker_sup:start_link().

stop(_State) ->
    ok.

set_path() ->
    compose:compose(
        [
            fun(DbPath) -> db:set_path(DbPath) end,
            fun(_) ->
                {ok, DbPath} = application:get_env(redmine_tracker, db_path),
                true = is_binary(DbPath),
                DbPath
            end
        ],
        ok
    ).


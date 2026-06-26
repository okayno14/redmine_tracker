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

start(_StartType, _StartArgs) ->
    logger:set_module_level(track, debug),
    %% When socket_address can't be expanded, we must fail without mnesia starting
    %% TODO переименовать в socket_address
    Socket = get_socket(),
    ok = set_path(),
    ok = db:start(),
    ok = db:ensure_all_ready(),
    ok = db:init(),
    redmine_tracker_sup:start_link(Socket).

stop(_State) ->
    ok.

set_path() ->
    compose:compose(
        [
            fun(DbPath) -> db:set_path(DbPath) end,
            fun(DbPath) -> characters:to_list(DbPath) end,
            fun(DbPath) ->
                %% if {nomatch, error} => user configured environment variable, that doesn't exist
                case
                    {
                        string:find(DbPath, <<"$XDG_STATE_HOME">>, leading),
                        path:expand(DbPath)
                    }
                of
                    %% XDG_STATE_HOME can be undefined
                    {Str, error} when is_binary(Str) -> <<"~/.local/state/redmine_tracker">>;
                    {_, {ok, DbPath2}} -> DbPath2
                end
            end,
            fun(_) ->
                {ok, DbPath} = application:get_env(redmine_tracker, db_path),
                true = is_binary(DbPath),
                DbPath
            end
        ],
        ok
    ).

get_socket() ->
    {ok, Socket} = application:get_env(redmine_tracker, socket),
    true = is_binary(Socket),
    {ok, Socket2} = path:expand(Socket),
    Socket2.


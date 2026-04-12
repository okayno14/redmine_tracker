-module(db).

-include_lib("kernel/include/logger.hrl").

-export([
    start/0,
    init/0,
    ensure_all_ready/0,
    transaction/1
]).

start() ->
    mnesia:create_schema([erlang:node()], []),
    mnesia:start(),
    ok = track:create_table().

init() ->
    true = either:is_right(transaction(fun track:init_tables/0)),
    ok.

ensure_all_ready() ->
    ?LOG_INFO("waiting for tables initialization, ..."),
    track:wait_for_tables(),
    ?LOG_INFO("all tables initialized successfully!!!"),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% <pre>
%% Transaction db wrapper.
%% If fun returns value -> transaction finished successfully.
%% Fun can do erlang:throw or erlang:error.
%% erlang:exit is forbidden (cause mnesia used them for communication)!
%% </pre>
%% @end
-spec transaction(Fun :: fun(() -> Res)) ->
    either:either(
        {throw, Reason :: dynamic()}
        | {Reason2 :: dynamic(), StackTrace :: erlang:stacktrace()},
        Res
    ).
%%--------------------------------------------------------------------
transaction(Fun) ->
    case mnesia:transaction(Fun) of
        {atomic, Res} -> either:right(Res);
        %% cause mnesia:transaction/1 doesn't have propper spec, then we match exactly results for fail-fast debug
        {aborted, {throw, _Reason} = Res} -> either:left(Res);
        %% cause mnesia:transaction/1 doesn't have propper spec, then we match exactly results for fail-fast debug
        {aborted, {_Reason, StackTrace} = Res} when is_list(StackTrace) -> either:left(Res)
    end.
%%--------------------------------------------------------------------


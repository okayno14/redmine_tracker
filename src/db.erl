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
    {atomic, ok} = transaction(fun track:init_tables/0),
    ok.

ensure_all_ready() ->
    ?LOG_INFO("waiting for tables initialization, ..."),
    track:wait_for_tables(),
    ?LOG_INFO("all tables initialized successfully!!!"),
    ok.

transaction(Fun) ->
    mnesia:transaction(Fun).


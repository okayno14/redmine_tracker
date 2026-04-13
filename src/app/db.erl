-module(db).

-include_lib("kernel/include/logger.hrl").

-export([
    start/0,
    init/0,
    ensure_all_ready/0,
    transaction/1,
    either_throw/1
]).

-export_type([
    transaction_ret/0,
    transaction_ret/1,
    transaction_ret/2
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
-type transaction_ret() ::
    transaction_ret(dynamic()).

-type transaction_ret(Res) ::
    transaction_ret(Err :: dynamic(), Res).

%% describes Result and user's specified Error
-type transaction_ret(Err, Res) ::
    either:either(
        %% if throw happened in transaction
        {throw, Err}
        %% if error happened in transaction
        | {Reason :: dynamic(), StackTrace :: erlang:stacktrace()}
        %% unknown mnesia exception
        | Reason :: dynamic(),
        Res
    ).

-spec transaction(Fun :: fun(() -> Res)) ->
    transaction_ret(Res).
%%--------------------------------------------------------------------
transaction(Fun) ->
    case mnesia:transaction(Fun) of
        {atomic, Res} ->
            either:right(Res);
        {aborted, {throw, _Reason} = Res} ->
            either:left(Res);
        {aborted, {_Reason, StackTrace} = Res} when is_list(StackTrace) ->
            either:left(Res);
        {aborted, Reason} ->
            either:left(Reason)
    end.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% <pre>
%% For aborting db transaction.
%% Allows to bypass either type from transaction to transaction-caller with using db:transaction/1
%% </pre>
%% @end
-spec either_throw(Either :: either:either(_Error, Ok)) ->
    Ok.
%%--------------------------------------------------------------------
either_throw(Either) ->
    compose:if_else(
        fun either:is_right/1,
        fun either:extract/1,
        fun(X2) -> erlang:throw(either:extract(X2)) end,
        Either
    ).
%%--------------------------------------------------------------------


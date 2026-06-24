-module(db).

-include_lib("kernel/include/logger.hrl").

-export([
    set_path/1,
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

set_path(DbPath) ->
    compose:compose(
        [
            fun(DbPath2) -> application:set_env(mnesia, dir, DbPath2) end,
            fun(DbPath2) -> ok = filelib:ensure_dir(DbPath2), DbPath2 end,
            fun(DbPath2) -> characters_to_list(DbPath2) end,
            fun(DbPath2) -> {ok, DbPath3} = expand_path(DbPath2), DbPath3 end
        ],
        DbPath
    ).

start() ->
    Node = erlang:node(),
    case mnesia:create_schema([Node], []) of
        ok -> ok;
        {error, Reason = {_, {already_exists, _}}} -> ok
    end,
    mnesia:start(),
    ok = track:create_table(Node).

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

-spec expand_path(Path :: unicode:unicode_binary()) ->
    {ok, unicode:unicode_binary()} | error.
expand_path(Path) ->
    [Root | T] = string:split(Path, <<"/">>, leading),
    case expand_var(Root) of
        {ok, Root2} ->
            {ok, characters_to_binary([Root2, <<"/">>, T])};
        error ->
            error
    end.

-spec expand_var(unicode:unicode_binary()) ->
    {ok, unicode:unicode_binary()} | error.
expand_var(<<"~">>) ->
    expand_var(<<"$HOME">>);
expand_var(<<"$", Var/binary>>) ->
    compose:if_else(
        fun(Val) -> erlang:is_list(Val) end,
        fun(Val) -> {ok, characters_to_binary(Val)} end,
        fun(_Val) -> error end,
        os:getenv(characters_to_list(Var))
    );
expand_var(X) ->
    {ok, X}.

characters_to_list(Chars) ->
    Ret = unicode:characters_to_list(Chars),
    true = erlang:is_list(Ret),
    Ret.

characters_to_binary(Chars) ->
    Ret = unicode:characters_to_binary(Chars),
    true = erlang:is_binary(Ret),
    Ret.


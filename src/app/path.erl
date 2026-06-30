-module(path).

-export([expand/1]).

-spec expand(Path :: unicode:unicode_binary()) ->
    {ok, unicode:unicode_binary()} | error.
expand(Path) ->
    [Root | T] = string:split(Path, <<"/">>, leading),
    case expand_var(Root) of
        {ok, Root2} ->
            {ok, characters:to_binary([Root2, <<"/">>, T])};
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
        fun(Val) -> {ok, characters:to_binary(Val)} end,
        fun(_Val) -> error end,
        os:getenv(characters:to_list(Var))
    );
expand_var(X) ->
    {ok, X}.


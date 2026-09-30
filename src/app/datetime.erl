-module(datetime).

-export([is_datetime/1, to_binary/1, from_binary/1, at_one_date/2]).

%%--------------------------------------------------------------------
is_datetime({{Y, Month, D}, {H, Min, S}})
when
    is_integer(Y),
    is_integer(Month), Month >= 1, Month =< 12,
    is_integer(D), D >= 1, D =< 31,
    is_integer(H), H >= 0, H =< 23,
    is_integer(Min), Min >= 0, Min =< 59,
    is_integer(S), S >= 0, S =< 59
->
    true;
is_datetime(_) ->
    false.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
at_one_date(DateTimeA, DateTimeB) ->
    {{Y1, M1, D1}, _} = DateTimeA,
    {{Y2, M2, D2}, _} = DateTimeB,
    Y1 == Y2, M1 == M2, D1 == D2.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
to_binary(DateTime) ->
    {{Y, Month, D}, {H, Min, S}} = DateTime,
    FormatNumber =
        fun
            (X) when is_integer(X), X >= 0, X =< 9 -> io_lib:format("0~p", [X]);
            (X) when is_integer(X), X > 9 -> io_lib:format("~p", [X])
        end,
    erlang:list_to_binary(
        io_lib:format("~p-~s-~s ~s:~s:~s", [
            Y,
            FormatNumber(Month),
            FormatNumber(D),
            FormatNumber(H),
            FormatNumber(Min),
            FormatNumber(S)
        ])
    ).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
-spec from_binary(DateTimeBin :: unicode:unicode_binary()) ->
    either:either(
        {error, {bad_datetime, Msg :: unicode:unicode_binary()}},
        calendar:datetime()
    ).
%%--------------------------------------------------------------------
from_binary(DateTimeBin) ->
    compose:compose(
        [
            fun(Either) ->
                either:flatmap(
                    Either,
                    fun({_Date, Time}) ->
                        try
                            either:right(
                                {_Date,
                                    erlang:list_to_tuple(
                                        lists:map(
                                            fun(X) when is_binary(X) -> erlang:binary_to_integer(X) end, Time
                                        )
                                    )}
                            )
                        catch
                            _:_:_ ->
                                either:left(
                                    {error,
                                        {bad_datetime,
                                            <<"Time contains non-numeric symbols">>}}
                                )
                        end
                    end
                )
            end,
            fun(Either) ->
                either:flatmap(
                    Either,
                    fun({_Date, Time}) ->
                        case string:split(Time, <<":">>, all) of
                            L = [H, M, S] ->
                                either:right({_Date, L});
                            _ ->
                                either:left(
                                    {error, {bad_datetime, <<"Failed to split time">>}}
                                )
                        end
                    end
                )
            end,
            fun(Either) ->
                either:flatmap(
                    Either,
                    fun({Date, _Time}) ->
                        try
                            either:right({
                                erlang:list_to_tuple(
                                    lists:map(fun(X) when is_binary(X) -> erlang:binary_to_integer(X) end, Date)
                                ),
                                _Time
                            })
                        catch
                            _:_:_ ->
                                either:left(
                                    {error,
                                        {bad_datetime,
                                            <<"Date contains non-numeric symbols">>}}
                                )
                        end
                    end
                )
            end,
            fun(Either) ->
                either:flatmap(
                    Either,
                    fun({Date, _Time}) ->
                        case string:split(Date, <<"-">>, all) of
                            L = [Y, M, D] ->
                                either:right({L, _Time});
                            _ ->
                                either:left(
                                    {error, {bad_datetime, <<"Failed to split date">>}}
                                )
                        end
                    end
                )
            end,
            fun(Either) ->
                either:flatmap(
                    Either,
                    fun(X) ->
                        case
                            string:split(
                                string:trim(X, both, "\""), <<" ">>, all
                            )
                        of
                            [DateBin, TimeBin] ->
                                either:right({DateBin, TimeBin});
                            _ ->
                                either:left(
                                    {error,
                                        {bad_datetime,
                                            <<"Failed to split date and time">>}}
                                )
                        end
                    end
                )
            end
        ],
        either:right(DateTimeBin)
    ).
%%--------------------------------------------------------------------


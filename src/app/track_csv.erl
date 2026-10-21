-module(track_csv).

-behaviour(track_translator).

-export([
    export/1,
    import/1
]).

%%--------------------------------------------------------------------
-spec export([track:external_map()]) ->
    CSV :: unicode:unicode_binary().
%%--------------------------------------------------------------------
export([]) ->
    ~"";
export(L) ->
    H = #{} = erlang:hd(L),
    Args2 =
        lists:foldl(
            fun(Map = #{}, #{csv := CSV, timestamp := TS}) when is_list(CSV) ->
                Sep =
                    compose:if_else(
                        fun(Map2) ->
                            datetime:at_one_date(
                                maps:get(timestamp_begin, Map2),
                                TS
                            )
                        end,
                        fun(_) -> [~"\n"] end,
                        fun(_) -> [~"\n", ~"\n"] end,
                        Map
                    ),
                #{
                    csv => [export_(Map), Sep | CSV],
                    timestamp => maps:get(timestamp_begin, Map)
                }
            end,
            #{
                csv => [],
                timestamp => maps:get(timestamp_begin, H)
            },
            L
        ),
    #{csv := L2 = [_ | _]} = Args2,
    [_ | T] = lists:reverse(L2),
    % eqwalizer:ignore
    Ret = unicode:characters_to_binary(T),
    true = is_binary(Ret),
    Ret.

export_(Map) ->
    #{
        id := Id,
        project_id := ProjectID,
        activity := {_ActivityID, Activity},
        state := State,
        task := Task,
        timestamp_begin := TsBegin,
        timestamp_end := TsEnd,
        desc := Desc
    } = Map,

    IDBin = erlang:integer_to_binary(Id),
    StateBin = erlang:atom_to_binary(State),
    TaskBin = Task,
    TsBeginBin = datetime:to_binary(TsBegin),
    TsEndBin = datetime:to_binary(TsEnd),
    DescBin = Desc,
    <<
        IDBin/binary, ",",
        "\"", ProjectID/binary, "\"", ",",
        "\"", TaskBin/binary, "\"", ",",
        Activity/binary,",",
        "\"", TsBeginBin/binary, "\"", ",",
        "\"", TsEndBin/binary, "\"", ",",
        "\"", DescBin/binary, "\"", ",",
        StateBin/binary, ";"
    >>.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
-spec import(CSV :: unicode:unicode_binary()) ->
    either:either(
        {error, Reason :: unicode:unicode_binary()},
        [track:external_map()]
    ).
%% TODO добавить в аргумент activities
%% TODO перетащить тело функции из track сюда
%% TODO сделать конвертацию ошибок в binary - человекочитаемой строчке
%% TODO перетащить тесты
%%--------------------------------------------------------------------
import(Bin) ->
    either:flatmap(track:activities(), fun(Activities) ->
        from_csv(Bin, Activities)
    end).

-type from_csv_all_err() ::
    {error, bad_csv}
    | {error, {bad_csv, Line :: unicode:unicode_binary()}, track:from_csv_err()}.

-spec from_csv(CSV :: unicode:unicode_binary(), Activities :: track:activites()) ->
    either:either(
        from_csv_all_err(),
        [track:track()]
    ).
from_csv(CSV, Activities) ->
    lists:foldl(
        fun
            (<<>>, Either) ->
                either:map(Either, fun lists:reverse/1);
            (Line, Either) when is_binary(Line) ->
                either:flatmap(
                    Either,
                    fun
                        (Acc) ->
                            Ret = from_csv_(Line, Activities),
                            case either:is_left(Ret) of
                                true ->
                                    either:left(
                                        {error,
                                            {bad_csv, Line},
                                            either:extract(Ret)
                                        }
                                    );
                                false ->
                                    either:right(
                                        [either:extract(Ret) | Acc]
                                    )
                            end
                    end
                )
        end,
        either:right([]),
        string:split(CSV, <<"\n">>, all)
    ).

-type from_csv_err() ::
    {error, state, {error, Msg :: unicode:unicode_binary()}}
    | {error, timestamp_end, {error, {bad_datetime, Msg :: unicode:unicode_binary()}}}
    | {error, timestamp_begin, {error, {bad_datetime, Msg :: unicode:unicode_binary()}}}
    | {error, {id, Msg :: unicode:unicode_binary()}}
    | {error, activity, {error, not_found}}
    | {error, {bad_csv, Msg :: unicode:unicode_binary()}}.

-spec from_csv_(CSV :: unicode:unicode_binary(), Activities :: track:activites()) ->
    either:either(
        from_csv_err(),
        track:track()
    ).
%% TODO отрефакторить: поделить на шаги, переименовать переменные
from_csv_(CSV, Activities) ->
    compose:compose(
        [
            fun(Either) ->
                either:map(
                    Either,
                    fun([IDBin, ProjectID, TaskBin, Activity, TsBeginBin, TsEndBin, DescBin, StateBin]) ->
                        #{
                            id => IDBin,
                            project_id => ProjectID,
                            activity => Activity,
                            task => TaskBin,
                            timestamp_begin => TsBeginBin,
                            timestamp_end => TsEndBin,
                            desc => DescBin,
                            state => StateBin
                        }
                    end
                )
            end,
            fun(Either) ->
                either:flatmap(
                    Either,
                    fun
                        ([IDBin, ProjectID, TaskBin, Activity, TsBeginBin, TsEndBin, DescBin, StateBin])
                        when
                            StateBin == <<"tracking">>; StateBin == <<"finished">>
                        ->
                            either:right([IDBin, ProjectID, TaskBin, Activity, TsBeginBin, TsEndBin, DescBin, erlang:binary_to_atom(StateBin)]);
                        ([IDBin, ProjectID, TaskBin, Activity, TsBeginBin, TsEndBin, DescBin, StateBin]) ->
                            either:left({error, state, {error, <<"State must be \"finished\" or \"tracking\"">>}})
                    end
                )
            end,
            fun(Either) ->
                either:flatmap(
                    Either,
                    %% TODO переименовать с Bin на просто ID
                    fun([IDBin, ProjectID, TaskBin, Activity, TsBeginBin, TsEndBin, DescBin, StateBin]) ->
                        X = datetime:from_binary(TsEndBin),
                        case either:is_left(X) of
                            true -> either:left({error, timestamp_end, either:extract(X)});
                            false -> either:right([IDBin, ProjectID, TaskBin, Activity, TsBeginBin, either:extract(X), DescBin, StateBin])
                        end
                    end
                )
            end,
            fun(Either) ->
                either:flatmap(
                    Either,
                    %% TODO переименовать с Bin на просто ID
                    fun([IDBin, ProjectID, TaskBin, Activity, TsBeginBin, TsEndBin, DescBin, StateBin]) ->
                        X = datetime:from_binary(TsBeginBin),
                        case either:is_left(X) of
                            true -> either:left({error, timestamp_begin, either:extract(X)});
                            false -> either:right([IDBin, ProjectID, TaskBin, Activity, either:extract(X), TsEndBin, DescBin, StateBin])
                        end
                    end
                )
            end,
            fun(Either) ->
                either:flatmap(
                    Either,
                    fun([IDBin, ProjectID, TaskBin, Activity, TsBeginBin, TsEndBin, DescBin, StateBin]) ->
                        try
                            either:right([erlang:binary_to_integer(IDBin), ProjectID, TaskBin, Activity, TsBeginBin, TsEndBin, DescBin, StateBin])
                        catch
                            Class:Reason:StackTrace ->
                                either:left({error, {id, <<"Id contains non-numeric symbols">>}})
                        end
                    end
                )
            end,
            fun(Either) ->
                either:flatmap(
                    Either,
                    fun([IDBin, ProjectID, TaskBin, ActivityDesc, TsBeginBin, TsEndBin, DescBin, StateBin]) ->
                        either:cata(
                            track:activity(ActivityDesc, Activities),
                            fun(_) -> {error, activity, {error, not_found}} end,
                            fun(Activity) -> [IDBin, ProjectID, TaskBin, Activity, TsBeginBin, TsEndBin, DescBin, StateBin] end
                        )
                    end
                )
            end,
            %% Any csv field can be enclosed in double quotes
            fun(Either) ->
                either:map(
                    Either,
                    fun(L) -> lists:map(fun(X) -> string:trim(X, both, "\"") end, L) end
                )
            end,
            fun(Either) ->
                either:flatmap(
                    Either,
                    fun
                        (X = [_, _, _, _, _, _, _, _]) -> either:right(X);
                        (_) -> either:left({error, {bad_csv, <<"Failed to split csv-line">>}})
                    end
                )
            end,
            fun(Either) ->
                either:map(Either, fun(X) -> string:split(X, <<",">>, all) end)
            end,
            fun(Either) ->
                either:map(Either, fun(X) -> string:trim(X, trailing, ";") end)
            end
        ],
        either:right(CSV)
    ).
%%--------------------------------------------------------------------


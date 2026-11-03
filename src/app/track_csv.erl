-module(track_csv).

-behaviour(track_translator).

-export([
    export/1,
    import/2
]).

-type from_csv_reason() ::
    {state, Msg :: unicode:unicode_binary()}
    | {timestamp_end, {bad_datetime, Msg :: unicode:unicode_binary()}}
    | {timestamp_begin, {bad_datetime, Msg :: unicode:unicode_binary()}}
    | {id, Msg :: unicode:unicode_binary()}
    | {activity, not_found}
    | {bad_csv, Msg :: unicode:unicode_binary()}.

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
-spec import(CSV :: unicode:unicode_binary(), Activities :: track:activites()) ->
    either:either(
        {error, Reason :: unicode:unicode_binary()},
        [track:external_map()]
    ).
%% TODO перетащить тесты
%%--------------------------------------------------------------------
import(Bin, Activities) ->
    compose:compose(
        [
            fun import_format_err/1,
            fun(X) -> import_from_csv(X, Bin) end
        ],
        either:right(Activities)
    ).

import_format_err(X) ->
    either:leftmap(X, fun format_import_error/1).

import_from_csv(X, Bin) ->
    either:flatmap(X, fun(Activities) -> from_csv(Bin, Activities) end).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
-spec from_csv(CSV :: unicode:unicode_binary(), Activities :: track:activites()) ->
    either:either(
        {error, from_csv_reason()},
        [track:track()]
    ).
from_csv(CSV, Activities) ->
    X =
        lists:foldl(
            fun
                (~"", Either) -> Either;
                (Line, Either) when is_binary(Line) ->
                    either:flatmap(
                        Either,
                        fun(Acc) ->
                            either:cata(
                                from_csv_(Line, Activities),
                                fun({error, Reason}) -> {error, {Reason, Line}} end,
                                fun(Map) -> [Map | Acc] end
                            )
                        end
                    )
            end,
            either:right([]),
            string:split(CSV, <<"\n">>, all)
        ),
    either:map(X, fun lists:reverse/1).

-spec from_csv_(CSV :: unicode:unicode_binary(), Activities :: track:activites()) ->
    either:either(
        from_csv_reason(),
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
                            either:left({error, {state, ~"State must be \"finished\" or \"tracking\""}})
                    end
                )
            end,
            fun(Either) ->
                either:flatmap(
                    Either,
                    %% TODO переименовать с Bin на просто ID
                    fun([IDBin, ProjectID, TaskBin, Activity, TsBeginBin, TsEndBin, DescBin, StateBin]) ->
                        either:cata(
                            datetime:from_binary(TsEndBin),
                            fun({error, Reason}) -> {error, {timestamp_end, Reason}} end,
                            fun(TsEndBin2) -> [IDBin, ProjectID, TaskBin, Activity, TsBeginBin, TsEndBin2, DescBin, StateBin] end
                        )
                    end
                )
            end,
            fun(Either) ->
                either:flatmap(
                    Either,
                    %% TODO переименовать с Bin на просто ID
                    fun([IDBin, ProjectID, TaskBin, Activity, TsBeginBin, TsEndBin, DescBin, StateBin]) ->
                        either:cata(
                            datetime:from_binary(TsBeginBin),
                            fun({error, Reason}) -> {error, {timestamp_begin, Reason}} end,
                            fun(TsBeginBin2) -> [IDBin, ProjectID, TaskBin, Activity, TsBeginBin2, TsEndBin, DescBin, StateBin] end
                        )
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
                                either:left({error, {id, ~"Id contains non-numeric symbols"}})
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
                            fun(_) -> {error, {activity, not_found}} end,
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
                        (_) -> either:left({error, {bad_csv, ~"Failed to split csv-line"}})
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

%%--------------------------------------------------------------------
-spec format_import_error(
    {error, {from_csv_reason(), Line :: unicode:unicode_binary()}}
) ->
    {error, Reason :: unicode:unicode_binary()}.

format_import_error({error, {Reason, Line}}) ->
    {
        error,
        unicode:characters_to_binary([
            ~"Error: ", format_from_csv_reason(Reason), ~"\n",
            ~"csv-line: ", Line
        ])
    }.

format_from_csv_reason({state, Msg}) ->
    <<"state: ", Msg/binary>>;
format_from_csv_reason({timestamp_end, {bad_datetime, Msg}}) ->
    <<"timestamp_end: ", Msg/binary>>;
format_from_csv_reason({timestamp_begin, {bad_datetime, Msg}}) ->
    <<"timestamp_begin: ", Msg/binary>>;
format_from_csv_reason({id, Msg}) ->
    <<"id: ", Msg/binary>>;
format_from_csv_reason({activity, not_found}) ->
    ~"activity: not found";
format_from_csv_reason({bad_csv, Msg}) ->
    <<"bad csv: ", Msg/binary>>.
%%--------------------------------------------------------------------


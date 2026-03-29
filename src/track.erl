-module(track).

-include_lib("kernel/include/logger.hrl").

-define(
    R2L(Record, RecordName),
    [
        {Field, element(Pos, Record)}
        || {Pos, Field} <-
            lists:zip(
                lists:seq(2, record_info(size, RecordName)),
                record_info(fields, RecordName)
            )
    ]
).

%% clean functions
-export([
    make/6,
    make/7,
    new/8,
    validate/1,
    %% создать рекорд
    %% track
    %% обновить отметку окончания трекаинга
    %% end_track
    from_csv/2,
    from_csv_all/2,
    to_csv/1,

    activity/2
]).

%% dirty functions
-export([
    activities/0,
	push_to_redmine/4
    %% db-операции
    %% get
    %% set
    %% delete
]).

-eqwalizer({nowarn_function, from_csv_all/2}).
-eqwalizer({nowarn_function, validate/1}).
-eqwalizer({nowarn_function, push_to_redmine_test_/0}).
-eqwalizer({nowarn_function, activities/0}).

-record(track, {
    id :: pos_integer(),
    project_id :: unicode:unicode_binary(),
    activity :: activity(),
    state :: track_state(),
    task :: unicode:unicode_binary(),
    timestamp_begin :: calendar:datetime(),
    timestamp_end :: calendar:datetime(),
    desc :: unicode:unicode_binary()
}).

-export_type([
    track/0
]).

-opaque track() :: #track{}.

-type track_state() :: tracking | finished.

-type activites() ::
    #{
        ActivityDesc :: unicode:unicode_binary() => ActivityID :: pos_integer()
    }.

-type activity() :: {
    ActivityID :: pos_integer(), Activity :: unicode:unicode_binary()
}.

-spec make(
    Id :: pos_integer(),
    ProjectID :: unicode:unicode_binary(),
    Activity :: activity(),
    Task :: unicode:unicode_binary(),
    TsBegin :: calendar:datetime(),
    Desc :: unicode:unicode_binary()
) ->
    track().
make(Id, ProjectID, Activity = {_, _}, Task, TsBegin, Desc) ->
    make(Id, ProjectID, Activity, Task, TsBegin, TsBegin, Desc).

-spec make(
    Id :: pos_integer(),
    ProjectID :: unicode:unicode_binary(),
    Activity :: activity(),
    Task :: unicode:unicode_binary(),
    TsBegin :: calendar:datetime(),
    TsEnd :: calendar:datetime(),
    Desc :: unicode:unicode_binary()
) ->
    track().
make(Id, ProjectID, Activity = {_, _}, Task, TsBegin, TsEnd, Desc) ->
    new(Id, ProjectID, Activity, Task, TsBegin, TsEnd, Desc, tracking).

-spec new(
    Id :: pos_integer(),
    ProjectID :: unicode:unicode_binary(),
    Activity :: activity(),
    Task :: unicode:unicode_binary(),
    TsBegin :: calendar:datetime(),
    TsEnd :: calendar:datetime(),
    Desc :: unicode:unicode_binary(),
    State :: track_state()
) ->
    track().
new(Id, ProjectID, Activity = {_, _}, Task, TsBegin, TsEnd, Desc, State) ->
    #track{
        id = Id,
        project_id = ProjectID,
        activity = Activity,
        state = State,
        task = Task,
        timestamp_begin = TsBegin,
        timestamp_end = TsEnd,
        desc = Desc
    }.

%%--------------------------------------------------------------------
-spec validate(Track :: track()) ->
    ErrorList :: [
        validate_id_err()
        | validate_project_id_err()
        | validate_activity_err()
        | validate_state_err()
        | validate_task_err()
        | validate_timestamps_err()
        | validate_desc_err()
    ].
%%--------------------------------------------------------------------
validate(Track = #track{}) ->
    L = ?R2L(Track, track),
    ?LOG_DEBUG("~p", [L]),
    lists:reverse(
        lists:foldl(
            fun
                ({id, ID}, ErrorList) ->
                    validate_id(ID) ++ ErrorList;
                ({project_id, ProjectID}, ErrorList) ->
                    validate_project_id(ProjectID) ++ ErrorList;
                ({activity, Activity}, ErrorList) ->
                    validate_activity(Activity) ++ ErrorList;
                ({state, State}, ErrorList) ->
                    validate_state(State) ++ ErrorList;
                ({task, Task}, ErrorList) ->
                    validate_task(Task) ++ ErrorList;
                ({timestamp_begin, _}, ErrorList) ->
                    ErrorList;
                ({timestamp_end, _}, ErrorList) ->
                    validate_timestamps(Track) ++ ErrorList;
                ({desc, Desc}, ErrorList) ->
                    validate_desc(Desc) ++ ErrorList
            end,
            [],
            L
        )
    ).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
-type validate_id_err() ::
    {error, {id, ID :: term(), Msg :: unicode:unicode_binary()}}.

-spec validate_id(ID :: term()) ->
    ErrorList :: [validate_id_err()].
%%--------------------------------------------------------------------
validate_id(ID) when is_integer(ID), ID > 0 ->
	[];
validate_id(ID) ->
	[{error, {id, ID, <<"id not a valid integer">>}}].
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
-type validate_project_id_err() ::
    {error, {project_id, ProjectID :: term(), Msg :: unicode:unicode_binary()}}.

-spec validate_project_id(ProjectID :: term()) ->
    ErrorList :: [validate_project_id_err()].
%%--------------------------------------------------------------------
validate_project_id(ProjectID) when is_binary(ProjectID) ->
    case string:find(ProjectID, <<" ">>) of
        nomatch ->
            [];
        _ ->
            [{error, {project_id, ProjectID, <<"project_id contains spaces">>}}]
    end;
validate_project_id(ProjectID) ->
    [{error, {project_id, ProjectID, <<"project_id not a binary">>}}].
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
-type validate_activity_err() ::
    {error, {activity, ActivityID :: term(), Msg :: unicode:unicode_binary()}}.

-spec validate_activity(ActivityID :: term()) ->
    ErrorList :: [validate_activity_err()].
%%--------------------------------------------------------------------
validate_activity({ActivityID, ActivityDesc}) when
    is_integer(ActivityID), ActivityID > 0, is_binary(ActivityDesc)
->
    [];
validate_activity({ActivityID, _ActivityDesc}) ->
    [
        {error,
            {activity, ActivityID,
                <<"activity must be {ActivityID, ActivityDesc}">>}}
    ].
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
-type validate_state_err() ::
    {error, {state, State :: term(), Msg :: unicode:unicode_binary()}}.

-spec validate_state(State :: term()) ->
    [validate_state_err()].
%%--------------------------------------------------------------------
validate_state(tracking) ->
    [];
validate_state(finished) ->
    [];
validate_state(State) ->
    [{error, {state, State, <<"state must be finished or tracking">>}}].
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
-type validate_task_err() ::
    {error,
        {task, Task :: term(), Msg :: unicode:unicode_binary()},
        characters_to_list_err() | {error, no_integer | not_a_list}
    }
    | {error,
        {task, Task :: term(), Msg :: unicode:unicode_binary()}
    }.

-type characters_to_list_err() ::
    {error, string(),
        RestData ::
            unicode:latin1_chardata()
            | unicode:chardata()
            | unicode:external_chardata()}
    | {incomplete, string(), binary()}.

-spec validate_task(Task :: term()) ->
    [validate_task_err()].
%%--------------------------------------------------------------------
validate_task(Task) when is_binary(Task) ->
    compose:compose(
        [
            fun(Either) ->
                case either:is_left(Either) of
                    true -> [either:extract(Either)];
                    false -> []
                end
            end,
            fun(Either) ->
                either:flatmap(
                    Either,
                    fun({Int, Rest}) ->
                        case string:is_empty(Rest) of
                            true ->
                                either:right(ok);
                            false ->
                                either:left(
                                    {error, {task, Task, <<"task contains not numeric chars after integer">>}}
                                )
                        end
                    end
                )
            end,
            fun(Either) ->
                either:flatmap(
                    Either,
                    fun(X) ->
                        case string:list_to_integer(X) of
                            Err = {error, no_integer} ->
                                either:left({error, {task, Task, <<"Binary not a string representation of integer">>}, Err});
                            Err = {error, not_a_list} ->
                                either:left({error, {task, Task, <<"Binary not a string representation of integer">>}, Err});
                            {Int, Rest} ->
                                either:right({Int, Rest})
                        end
                    end
                )
            end,
            fun(Either) ->
                either:flatmap(
                    Either,
                    fun(X) ->
                        case unicode:characters_to_list(X) of
                            Err = {error, _, _} ->
                                either:left({error, {task, Task, <<"Binary not a string">>}, Err});
                            Err = {incomplete, _, _} ->
                                either:left({error, {task, Task, <<"Binary not a string">>}, Err});
                            String ->
                                either:right(String)
                        end
                    end
                )
            end
        ],
        either:right(Task)
    );
validate_task(Task) ->
    [{error, {task, Task, <<"task not a valid binary">>}}].
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
-type validate_timestamps_err() ::
    {error, {timestamp_begin, TsBegin :: term(), Msg :: unicode:unicode_binary()}}
    | {error, {timestamp_end, TsEnd :: term(), Msg :: unicode:unicode_binary()}}
    | {error, {timestamp, {TsBegin :: term(), TsEnd :: term()}, Msg :: unicode:unicode_binary()}}.

-spec validate_timestamps(Track :: term()) ->
    ErrorList :: [validate_timestamps_err()].
%%--------------------------------------------------------------------
validate_timestamps(Track) ->
    compose:compose(
        [
            fun(V) ->
                validation:extract_error_stack(V)
            end,
            fun(V) ->
                validation:ok_flatmap(
                    V,
                    fun(Track2) ->
                        case
                            seconds(
                                Track2#track.timestamp_begin,
                                Track2#track.timestamp_end
                            ) > 0
                        of
                            true ->
                                validation:validation(Track2);
                            false ->
                                validation:validation_error([
                                    {error,
                                        {timestamp,
                                            {
                                                Track2#track.timestamp_begin,
                                                Track2#track.timestamp_end
                                            },
                                            <<"timestamp_end - timestamp_begin must be positive">>
                                        }
                                    }
                                ])
                        end
                    end
                )
            end,
            fun(V) ->
                validation:flatmap(
                    V,
                    fun(Track2) ->
                        case is_datetime(Track2#track.timestamp_end) of
                            true ->
                                validation:validation(Track2);
                            false ->
                                validation:validation_error(
                                    [{error,
                                        {timestamp_end,
                                            Track2#track.timestamp_end,
                                            <<"timestamp_end not a valid datetime">>
                                        }
                                    }]
                                )
                        end
                    end
                )
            end,
            fun(V) ->
                validation:flatmap(
                    V,
                    fun(Track2) ->
                        case is_datetime(Track2#track.timestamp_begin) of
                            true ->
                                validation:validation(Track2);
                            false ->
                                validation:validation_error([
                                    {error,
                                        {timestamp_begin,
                                            Track2#track.timestamp_begin,
                                            <<"timestamp_begin not a valid datetime">>
                                        }
                                    }
                                ])
                        end
                    end
                )
            end
        ],
        validation:validation(Track)
    ).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
-type validate_desc_err() ::
    {error, {desc, Desc :: term(), Msg :: unicode:unicode_binary()}}.

-spec validate_desc(Desc :: term()) ->
    ErrorList :: [validate_desc_err()].
%%--------------------------------------------------------------------
validate_desc(Desc) when is_binary(Desc) -> [];
validate_desc(Desc) -> [{error, {desc, Desc, <<"desc is not a binary">>}}].
%%--------------------------------------------------------------------

%% начать трекать элемент по id
%% track
%% закончить трекать элемент по id
%% end_track

%%--------------------------------------------------------------------
-spec from_csv_all(CSV :: unicode:unicode_binary(), Activities :: activites()) ->
    either:either(
        {error, bad_csv}
        | {error, {bad_csv, Line :: unicode:unicode_binary()}, from_csv_err()},
        track()
    ).
%%--------------------------------------------------------------------
from_csv_all(CSV, Activities) ->
    lists:foldl(
        fun
            (<<>>, Either) ->
                either:map(Either, fun lists:reverse/1);
            (Line, Either) when is_binary(Line) ->
                either:flatmap(
                    Either,
                    fun
                        (Acc) ->
                            Ret = from_csv(Line, Activities),
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
%%--------------------------------------------------------------------

-type from_csv_err() ::
    {error, state, {error, Msg :: unicode:unicode_binary()}}
    | {error, timestamp_end, {error, {bad_datetime, Msg :: unicode:unicode_binary()}}}
    | {error, timestamp_begin, {error, {bad_datetime, Msg :: unicode:unicode_binary()}}}
    | {error, {id, Msg :: unicode:unicode_binary()}}
    | {error, activity, {error, not_found}}
    | {error, {bad_csv, Msg :: unicode:unicode_binary()}}.

-spec from_csv(CSV :: unicode:unicode_binary(), Activities :: activites()) ->
    either:either(
            from_csv_err(),
            track()
        ).
from_csv(CSV, Activities) ->
    compose:compose(
        [
            fun(Either) ->
                either:map(
                    Either,
                    fun([IDBin, ProjectID, TaskBin, Activity, TsBeginBin, TsEndBin, DescBin, StateBin]) ->
                        track:new(
                            IDBin,
                            ProjectID,
                            Activity,
                            TaskBin,
                            TsBeginBin,
                            TsEndBin,
                            DescBin,
                            StateBin
                        )
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
                        X = binary_to_datetime_2(TsEndBin),
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
                        X = binary_to_datetime_2(TsBeginBin),
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
                            activity(ActivityDesc, Activities),
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

%% экспортировать в csv-строку рекорд
-spec to_csv(Track :: track()) ->
    CSV :: unicode:unicode_binary().
to_csv(Track) ->
    #track{
        id = Id,
        project_id = ProjectID,
        activity = {_ActivityID, Activity},
        state = State,
        task = Task,
        timestamp_begin = TsBegin,
        timestamp_end = TsEnd,
        desc = Desc
    } = Track,
    IDBin = erlang:integer_to_binary(Id),
    StateBin = erlang:atom_to_binary(State),
    TaskBin = Task,
    TsBeginBin = datetime_to_binary(TsBegin),
    TsEndBin = datetime_to_binary(TsEnd),
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

-spec push_to_redmine(
    Track :: track(),
    UserId :: pos_integer(),
    RedmineInstance :: unicode:unicode_binary(),
    ApiKey :: unicode:unicode_binary()
) ->
    ok | {error, Reason :: term()}.
push_to_redmine(Track, UserId, RedmineInstance, ApiKey) ->
	%% TODO возможно лучше перетащить внутрь to_xml, т.к. я больше бинарями пользуюсь
	XML = unicode:characters_to_binary(to_xml(Track, UserId)),
    ?LOG_DEBUG("Parsed XML:~ts", [XML]),
	true = is_binary(XML),
    case
        httpc:request(
            post,
            {
                <<RedmineInstance/binary, "/time_entries.xml">>,
                [{"X-Redmine-API-Key", ApiKey}],
                "application/xml",
                XML
            },
            _HttpOptions = [
                {ssl, [{verify, verify_none}]},
                {timeout, 5000}
            ],
            _Options = []
        )
    of
        {ok, Resp} ->
            ?LOG_DEBUG("Response:~p", [Resp]),
            ok;
        {error, Reason} ->
            ?LOG_WARNING("Reason:~p", [Reason]),
            {error, Reason}
    end.

to_xml(Track, UserId) ->
    #track{
        project_id = ProjectID,
        activity = {ActivityID, _Activity},
        task = Task,
        timestamp_begin = TsBegin,
        timestamp_end = TsEnd,
        desc = Desc
    } = Track,
    lists:flatten(
        xmerl:export_simple(
            [
                {time_entry, [
					%% TODO переделать все конвертации строк/бинарей на unicode-модуль
                    {project_id, [erlang:binary_to_list(ProjectID)]},
                    {issue_id, [erlang:binary_to_list(Task)]},
                    {activity_id, [erlang:integer_to_list(ActivityID)]},
                    {user_id, [erlang:integer_to_list(UserId)]},
                    {hours, [
                        erlang:float_to_list(seconds(TsBegin, TsEnd), [{decimals, 1}])
                    ]},
                    {comments, [unicode:characters_to_list(Desc)]},
                    {spent_on, [
                        erlang:binary_to_list(
                            begin
                                [X | _] = string:split(datetime_to_binary(TsBegin), <<" ">>),
                                true = is_binary(X),
                                X
                            end
                        )
                    ]}
                ]}
            ],
            xmerl_xml
        )
    ).

-spec seconds(TsBegin :: calendar:datetime(), TsEnd :: calendar:datetime()) ->
    Seconds :: float().
seconds(TsBegin, TsEnd) ->
    (calendar:datetime_to_gregorian_seconds(TsEnd) -
        calendar:datetime_to_gregorian_seconds(TsBegin)) / 3600.

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

datetime_to_binary(DateTime) ->
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

-spec binary_to_datetime_2(DateTimeBin :: unicode:unicode_binary()) ->
    either:either(
        {error, {bad_datetime, Msg :: unicode:unicode_binary()}},
        calendar:datetime()
    ).
binary_to_datetime_2(DateTimeBin) ->
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


%%%===================================================================
%%% activities
%%%===================================================================

-spec activities() ->
    activites().
activities() ->
    {ok, Activities = #{}} = application:get_env(redmine_tracker, activities),
    Activities.

-spec activity(ActivityDesc :: unicode:unicode_binary(), Activities :: activites()) ->
    either:either({error, not_found}, activity()).
activity(ActivityDesc, Activities) ->
    case maps:get(ActivityDesc, Activities, not_found) of
        not_found ->
            either:left({error, not_found});
        ActivityID ->
            either:right({ActivityID, ActivityDesc})
    end.

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

to_csv_test() ->
    ?assertEqual(
        <<"1,\"Redmine Tracker\",\"228\",Code,\"2026-02-25 22:42:00\",\"2026-02-25 22:50:00\",\"tested csv-export\",tracking;">>,
        track:to_csv(
            track:make(
                1,
                <<"Redmine Tracker">>,
                {1, <<"Code">>},
                <<"228">>,
                {{2026, 02, 25}, {22, 42, 00}},
                {{2026, 02, 25}, {22, 50, 00}},
                <<"tested csv-export">>
            )
        )
    ).

from_csv_test() ->
    Activities =
        #{
            <<"Design">> => 8,
            <<"Code">> => 9,
            <<"Code Review">> => 90,
            <<"Analysis">> => 96,
            <<"Discuss">> => 10,
            <<"Test">> => 11,
            <<"Management">> => 12,
            <<"Documentation">> => 13,
            <<"Support">> => 14
        },
    Track =
        track:new(
            1,
            <<"Redmine Tracker">>,
            {9, <<"Code">>},
            <<"228">>,
            {{2026, 02, 25}, {22, 42, 00}},
            {{2026, 02, 25}, {22, 50, 00}},
            <<"tested csv-export">>,
            finished
        ),
    ?assertEqual(
        Track,
        either:extract(track:from_csv(track:to_csv(Track), Activities))
    ).

push_to_redmine_test_() ->
    {foreach,
        fun() -> meck:new(httpc) end,
        fun(_) -> meck:unload(httpc) end,
        [
            fun() ->
                meck:expect(
                    httpc,
                    request,
                    fun(_, _, _, _) ->
                        {ok, <<"ok">>}
                    end
                ),
                track:push_to_redmine(
                    track:make(
                        _Id = 1,
                        ProjectID = <<"Redmine Tracker">>,
                        _Activity = {1, <<"Code">>},
                        Task =  <<"239715">>,
                        _TSBegin = {{2026, 02, 25}, {21, 50, 00}},
                        _TSEnd = {{2026, 02, 25}, {22, 50, 00}},
                        Desc = <<"tested redmine-push">>
                    ),
                    UserID = 1234,
                    _RedmineInstance = <<"redmine.org">>,
                    ApiKey = <<"fjiajfeijf">>
                ),
                ?assertEqual(
                    post,
                    meck:capture(last, httpc, request, ['_', '_', '_', '_'], 1)
                ),
                {Uri, Headers, ContentType, Body} =
                meck:capture(last, httpc, request, ['_', '_', '_', '_'], 2),
                ?assertEqual(<<"redmine.org/time_entries.xml">>, Uri),
                ?assertEqual([{"X-Redmine-API-Key", ApiKey}], Headers),
                ?assertEqual("application/xml", ContentType),
                ?assert(string:equal(
                    io_lib:format(
                        "<?xml version=\"1.0\"?>"
                        "<time_entry>"
                            "<project_id>~ts</project_id>"
                            "<issue_id>~ts</issue_id>"
                            "<activity_id>"
                                "1"
                            "</activity_id>"
                            "<user_id>~p</user_id>"
                            "<hours>~p</hours>"
                            "<comments>~ts</comments>"
                            "<spent_on>~ts</spent_on>"
                        "</time_entry>",
                        [
                            ProjectID,
                            Task,
                            UserID,
                            1.0,
                            Desc,
                            "2026-02-25"
                        ]
                    ),
                    Body
                ))
            end
        ]
    }.

-endif.


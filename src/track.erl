-module(track).

-include_lib("kernel/include/logger.hrl").

-export([
    new/5,
    new/7,
    new/8,
    %% validate
    %% создать рекорд, положить в базу
    %% track
    %% обновить отметку окончания трекаинга
    %% end_track
    from_csv/1,
    from_csv_all/1,
    to_csv/1,
    push_to_redmine/4
    %% db-операции
    %% get
    %% set
    %% delete
]).

-eqwalizer({nowarn_function, from_csv_all/1}).
-eqwalizer({nowarn_function, push_to_redmine_test_/0}).

-record(track, {
    id :: pos_integer(),
    project_id :: unicode:unicode_binary(),
    activity :: {ActivityID :: pos_integer(), Activity :: unicode:unicode_binary()},
    %% TODO вынести в тип
    state :: tracking | finished,
    task :: unicode:unicode_binary(),
    timestamp_begin :: calendar:datetime(),
    timestamp_end :: calendar:datetime(),
    desc :: unicode:unicode_binary()
}).

-export_type([
    track/0
]).

-opaque track() :: #track{}.

%% TODO переименовать в make? Чтобы не было путаницы между конструктором и такими композициями
-spec new(
    Id :: pos_integer(),
    ProjectID :: unicode:unicode_binary(),
    ActivityDesc ::
        unicode:unicode_binary()
        | {ActivityID :: pos_integer(), ActivityDesc :: unicode:unicode_binary()},
    Task :: unicode:unicode_binary(),
    Desc :: unicode:unicode_binary()
) ->
    {error, activity_not_found} | track().
new(Id, ProjectID, Activity, Task, Desc) when is_binary(Activity) ->
    {ok, Activities = #{}} = application:get_env(redmine_tracker, activities),
    %% TODO заменить на вызов find_acivity_by_desc/1
    case maps:get(Activity, Activities, not_found) of
        not_found ->
            {error, activity_not_found};
        ActivityID when is_number(ActivityID) ->
            %% TODO может сдублировать код? Не нравятся эти прыжки по клаузам, обычно это потом тяжёло читать
            new(Id, ProjectID, {ActivityID, Activity}, Task, Desc)
    end;
new(Id, ProjectID, Activity = {_, _}, Task, Desc) ->
    T1 = calendar:universal_time(),
    new(Id, ProjectID, Activity, Task, T1, T1, Desc).

-spec new(
    Id :: pos_integer(),
    ProjectID :: unicode:unicode_binary(),
    Activity :: {
        ActivityID :: pos_integer(), ActivityDesc :: unicode:unicode_binary()
    },
    Task :: unicode:unicode_binary(),
    TsBegin :: calendar:datetime(),
    TsEnd :: calendar:datetime(),
    Desc :: unicode:unicode_binary()
) ->
    track().
new(Id, ProjectID, Activity = {_, _}, Task, TsBegin, TsEnd, Desc) ->
    new(Id, ProjectID, Activity, Task, TsBegin, TsEnd, Desc, tracking).

-spec new(
    Id :: pos_integer(),
    ProjectID :: unicode:unicode_binary(),
    Activity :: {
        ActivityID :: pos_integer(), ActivityDesc :: unicode:unicode_binary()
    },
    Task :: unicode:unicode_binary(),
    TsBegin :: calendar:datetime(),
    TsEnd :: calendar:datetime(),
    Desc :: unicode:unicode_binary(),
    State :: tracking | finished
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

%% TODO Идея в том, что мы позволяем через конструкторы и парсеры создать кривой экземпляр
%% но если была прошла валидация, то мы гарантируем, что инвариант соблюдается, а значит не будет ошибки при вызове других функций модуля
%% validate

%% начать трекать элемент по id
%% track
%% закончить трекать элемент по id
%% end_track

%%--------------------------------------------------------------------
-spec from_csv_all(CSV :: unicode:unicode_binary()) ->
    either:either(
        {error, bad_csv}
        | {error, {bad_csv, Line :: unicode:unicode_binary()}, from_csv_err()},
        track()
    ).
%%--------------------------------------------------------------------
from_csv_all(CSV) ->
    lists:foldl(
        fun
            (<<>>, Either) ->
                either:map(Either, fun lists:reverse/1);
            (Line, Either) when is_binary(Line) ->
                either:flatmap(
                    Either,
                    fun
                        (Acc) ->
                            Ret = from_csv(Line),
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

-spec from_csv(CSV :: unicode:unicode_binary()) ->
    either:either(
            from_csv_err(),
            track()
        ).
from_csv(CSV) ->
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
                        case find_acivity_by_desc(ActivityDesc) of
                            {error, not_found} ->
                                either:left({error, activity, {error, not_found}});
                            Activity = {_, _} ->
                                ?LOG_DEBUG("q"),
                                either:right([IDBin, ProjectID, TaskBin, Activity, TsBeginBin, TsEndBin, DescBin, StateBin])
                        end
                    end
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
    case
        httpc:request(
            post,
            {
                <<RedmineInstance/binary, "/time_entries.xml">>,
                [{"X-Redmine-API-Key", ApiKey}],
                "application/xml",
                to_xml(Track, UserId)
            },
            _HttpOptions = [
                {ssl, [{verify, verify_none}]}
            ],
            _Options = []
        )
    of
        {ok, Resp} ->
            ?LOG_DEBUG("Response:~p", [Resp]),
            ok;
        {error, Reason} ->
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
                    {project_id, [erlang:binary_to_list(ProjectID)]},
                    {issue_id, [erlang:binary_to_list(Task)]},
                    {activity_id, [erlang:integer_to_list(ActivityID)]},
                    {user_id, [erlang:integer_to_list(UserId)]},
                    {hours, [
                        erlang:float_to_list(seconds(TsBegin, TsEnd), [{decimals, 1}])
                    ]},
                    {comments, [erlang:binary_to_list(Desc)]},
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

binary_to_datetime(DateTimeBin) ->
    [DateBin, TimeBin] = string:split(string:trim(DateTimeBin, both, "\""), <<" ">>, all),
    Date = erlang:list_to_tuple(
        lists:map(
            fun erlang:binary_to_integer/1, string:split(DateBin, <<"-">>, all)
        )
    ),
    Time = erlang:list_to_tuple(
        lists:map(
            fun erlang:binary_to_integer/1, string:split(TimeBin, <<":">>, all)
        )
    ),
    {Date, Time}.

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
%%% app_db
%%%===================================================================

-spec find_acivity_by_desc(ActivityDesc :: unicode:unicode_binary()) ->
    {error, not_found}
    %% TODO вынести в отдельный тип, а то часто приходится копипастить
    | {ActivityID :: pos_integer(), ActivityDesc :: unicode:unicode_binary()}.
find_acivity_by_desc(ActivityDesc) ->
    {ok, Activities = #{}} = application:get_env(redmine_tracker, activities),
    case maps:get(ActivityDesc, Activities, not_found) of
        not_found -> {error, not_found};
        ActivityID when is_number(ActivityID) -> {ActivityID, ActivityDesc}
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
            track:new(
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

%% TODO написать нормальный тест для проверки
%% TODO сделать тестовый конфиг для логгера
from_csv_test() ->
    application:load(redmine_tracker),
    application:set_env(
        redmine_tracker,
        activities,
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
        }
    ),
    ?debugFmt(
        "~p\n",
        [
            track:from_csv(
                track:to_csv(
                    track:new(
                        1,
                        <<"Redmine Tracker">>,
                        {1, <<"Code">>},
                        <<"228">>,
                        {{2026, 02, 25}, {22, 42, 00}},
                        {{2026, 02, 25}, {22, 50, 00}},
                        <<"tested csv-export">>,
                        finished
                    )
                )
            )
        ]
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
                    track:new(
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


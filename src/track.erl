-module(track).

-include_lib("kernel/include/logger.hrl").

-export([
    new/4,
    new/6,
    %% validate
    %% создать рекорд, положить в базу
    %% track
    %% обновить отметку окончания трекаинга
    %% end_track
    %% создать рекорд из csv-строки
    %% from_csv
    to_csv/1,
    push_to_redmine/4
    %% get
    %% set
    %% delete
]).

-record(track, {
    id :: pos_integer(),
    activity :: {ActivityID :: pos_integer(), Activity :: binary()},
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

new(Id, Activity, Task, Desc) when is_binary(Activity) ->
    {ok, Activities} = application:get_env(redmine_tracker, activities),
    Activity2 =
    case maps:get(Activity, Activities, not_found) of
        not_found -> {error, activity_not_found};
        ActivityID -> {ActivityID, Activity}
    end,
    new(Id, Activity2, Task, Desc);
new(Id, Activity = {_, _}, Task, Desc) ->
    T1 = calendar:universal_time(),
    new(Id, Activity, Task, T1, T1, Desc).

new(Id, Activity = {_, _}, Task, TsBegin, TsEnd, Desc) ->
    #track{
        id = Id,
        activity = Activity,
        state = tracking,
        task = Task,
        timestamp_begin = TsBegin,
        timestamp_end = TsEnd,
        desc = Desc
    }.

%% начать трекать элемент по id
%% track
%% закончить трекать элемент по id
%% end_track

%% экспортировать в csv-строку рекорд
-spec to_csv(Track :: track()) ->
    CSV :: unicode:unicode_binary().
to_csv(Track) ->
    #track{
        id = Id,
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
    #track{
        % id = Id,
        %% TODO в xml идёт какой-то activity_id, надо понять, как он выглядит у нас
        activity = {ActivityID, _Activity},
        state = State,
        task = Task,
        timestamp_begin = TsBegin,
        timestamp_end = TsEnd,
        desc = Desc
    } = Track,
    TrackXml =
        lists:flatten(
            xmerl:export_simple(
                [
                    {time_entry, [
                        %% TODO надо добавить project_id в track, иначе - неоткуда брать
                        %% TODO временно можно захардкодить SSW, потом сделать так, чтобы мы искали по имени и сейвили локально в бд
                        % {project_id, ["ecss-10"]},
                        {project_id, ["time-tracking"]},
                        %% TODO надо добавить извлечение таски, либо закрепить в валидаторе так, чтобы был только номер задачи
                        {issue_id, [erlang:binary_to_list(Task)]},
                        {activity_id, [erlang:integer_to_list(ActivityID)]},
                        %% TODO надо брать из конфига, но в эту функцию параметр должен попадать через аргумент,
                        %% можно было бы через поле в сущности. Но приложение задумано как однопользовательское, поэтому и нет смысла хранить user_id
                        {user_id, [erlang:integer_to_list(UserId)]},
                        {hours, [
                            erlang:float_to_list(
                                %% TODO вынести в отдельную функцию
                                (calendar:datetime_to_gregorian_seconds(TsEnd) -
                                    calendar:datetime_to_gregorian_seconds(TsBegin)) /
                                    3600,
                                [{decimals, 1}]
                            )
                        ]},
                        {comments, [erlang:binary_to_list(Desc)]},
                        {spent_on, [
                            erlang:binary_to_list(
                                erlang:hd(
                                    string:split(
                                        datetime_to_binary(TsBegin), <<" ">>
                                    )
                                )
                            )
                        ]}
                    ]}
                ],
                xmerl_xml
            )
        ),

    ?LOG_DEBUG("Body:~ts", [TrackXml]),

    case
        httpc:request(
            post,
            {
                <<RedmineInstance/binary, "/time_entries.xml">>,
                [{"X-Redmine-API-Key", ApiKey}],
                "application/xml",
                TrackXml
            },
            %% TODO пока доверяем сертификату
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

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

to_csv_test() ->
    ?assertEqual(
        <<"1,\"228\",Code,\"2026-02-25 22:42:00\",\"2026-02-25 22:50:00\",\"tested csv-export\",tracking;">>,
        track:to_csv(
            track:new(
                1,
                {1, <<"Code">>},
                <<"228">>,
                {{2026, 02, 25}, {22, 42, 00}},
                {{2026, 02, 25}, {22, 50, 00}},
                <<"tested csv-export">>
            )
        )
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
                            "<project_id>"
                                "time-tracking"
                            "</project_id>"
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


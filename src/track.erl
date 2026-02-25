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
    push_to_redmine/1
    %% get
    %% set
    %% delete
]).

-record(track, {
    id :: pos_integer(),
    %% TODO вынести в тип
    activity ::
        design
        | code
        | code_review
        | analysis
        | discuss
        | test
        | management
        | documentation
        | support,
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

new(Id, Activity, Task, Desc) ->
    T1 = calendar:universal_time(),
    new(Id, Activity, Task, T1, T1, Desc).

new(Id, Activity, Task, TsBegin, TsEnd, Desc) ->
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
        activity = Activity,
        state = State,
        task = Task,
        timestamp_begin = TsBegin,
        timestamp_end = TsEnd,
        desc = Desc
    } = Track,
    IDBin = erlang:integer_to_binary(Id),
    ActivityBin = erlang:atom_to_binary(Activity),
    StateBin = erlang:atom_to_binary(State),
    TaskBin = Task,
    TsBeginBin = datetime_to_binary(TsBegin),
    TsEndBin = datetime_to_binary(TsEnd),
    DescBin = Desc,
    <<
        IDBin/binary, ",",
        "\"", TaskBin/binary, "\"", ",",
        ActivityBin/binary,",",
        "\"", TsBeginBin/binary, "\"", ",",
        "\"", TsEndBin/binary, "\"", ",",
        "\"", DescBin/binary, "\"", ",",
        StateBin/binary, ";"
    >>.

-spec push_to_redmine(Track :: track()) ->
    ok | {error, Reason :: term()}.
push_to_redmine(Track) ->
    #track{
        % id = Id,
        %% TODO в xml идёт какой-тот activity_id, надо понять, как он выглядит у нас
        activity = Activity,
        state = State,
        task = Task,
        timestamp_begin = TsBegin,
        timestamp_end = TsEnd,
        desc = Desc
    } = Track,
    RedmineInstance = "red.eltex.loc/time_entries.xml",
    TrackXml =
        xmerl:export_simple(
            [
                {time_entry, [
                    %% TODO надо добавить project_id в track, иначе - неоткуда брать
                    %% TODO временно можно захардкодить SSW, потом сделать так, чтобы мы искали по имени и сейвили локально в бд
                    {project_id, ["228"]},
                    %% TODO надо добавить извлечение таски, либо закрепить в валидаторе так, чтобы был только номер задачи
                    {issue_id, [erlang:binary_to_list(Task)]},
                    %% TODO надо брать из конфига, но в эту функцию параметр должен попадать через аргумент
                    {user_id, ["1337"]},
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
                                string:split(datetime_to_binary(TsBegin), <<" ">>)
                            )
                        )
                    ]}
                ]}
            ],
            xmerl_xml
        ),

    ?LOG_ERROR("~ts", [lists:flatten(TrackXml)]),
    ok.
    % case
    %     httpc:request(
    %         post,
    %         {RedmineInstance, [], <<"application/xml">>, TrackXml},
    %         HttpOptions = [],
    %         Options = []
    %     )
    % of
    %     {ok, _} -> ok;
    %     {error, Reason} -> {error, Reason}
    % end.

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
        <<"1,\"refs #228\",code,\"2026-02-25 22:42:00\",\"2026-02-25 22:50:00\",\"tested csv-export\",tracking;">>,
        track:to_csv(
            track:new(
                1,
                code,
                <<"refs #228">>,
                {{2026, 02, 25}, {22, 42, 00}},
                {{2026, 02, 25}, {22, 50, 00}},
                <<"tested csv-export">>
            )
        )
    ).

-endif.


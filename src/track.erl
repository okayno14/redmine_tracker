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
    to_csv/1
    %% положить через redmine api трек
    %% push_to_redmine
    %% get
    %% set
    %% delete
]).

-record(track, {
    id :: pos_integer(),
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


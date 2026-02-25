-module(track).

-export([
    new/3,
    new/5
    %% validate
    %% создать рекорд, положить в базу
    %% track
    %% обновить отметку окончания трекаинга
    %% end_track
    %% создать рекорд из csv-строки
    %% from_csv
    %% экспортировать в csv-строку рекорд
    %% to_csv
    %% положить через redmine api трек
    %% push_to_redmine
    %% get
    %% set
    %% delete
]).

-record(track, {
    id :: pos_integer(),
    state :: tracking | finished,
    task :: unicode:chardata(),
    timestamp_begin :: calendar:datetime(),
    timestamp_end :: calendar:datetime(),
    desc :: unicode:chardata()
}).

-export_type([
    track/0
]).

-opaque track() :: #track{}.

new(Id, Task, Desc) ->
    T1 = calendar:universal_time(),
    new(Id, Task, T1, T1, Desc).

new(Id, Task, TsBegin, TsEnd, Desc) ->
    #track{
        id = Id,
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

%% id, state, date, begin, end, task, desc;
%% to csv


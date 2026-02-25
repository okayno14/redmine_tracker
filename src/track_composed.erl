-module(track_composed).

-export([
    %% если последний трекинг завершённый, то заводим новый
    % begin_track
    %% если последний трекинг незавершённый, то завершаем его
    % end_track_last
    %% экспортировать всю базу в csv
    % export_to_csv
    %% переписать всю базу содержим csv
    %% import_from csv
    %% очистить всю базу и отправить в redmine_api
    %% push_to_redmine
]).

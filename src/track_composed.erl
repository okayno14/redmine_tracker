-module(track_composed).

-include_lib("kernel/include/logger.hrl").

-export([
    begin_track/4,
    %% если последний трекинг незавершённый, то завершаем его
    end_track_last/0,
    %% экспортировать всю базу в csv
    export_to_csv/0
    %% переписать всю базу содержим csv
    %% import_from csv
    %% очистить всю базу и отправить в redmine_api
    %% push_to_redmine
]).

%% TODO проработать формат ошибок
%% Создать новый объект track в состоянии tracking с текущим системным временем начала
%% Положить в базу
begin_track(ProjectID, ActivityDesc, Task, Desc) ->
    F = fun() ->
        compose:compose(
            [
                fun either_throw/1,
                fun(X) -> either:map(X, fun track:set/1) end,
                fun(X) ->
                    either:flatmap(
                        X,
                        fun(Track) ->
                            compose:if_else(
                                fun(ErrorList) -> ErrorList =:= [] end,
                                fun(_) -> either:right(Track) end,
                                fun either:left/1,
                                track:validate(Track)
                            )
                        end
                    )
                end,
                fun(X) ->
                    either:map(
                        X,
                        fun(Activity) ->
                            track:make(
                                track:next_id(),
                                ProjectID,
                                Activity,
                                Task,
                                calendar:local_time(),
                                Desc
                            )
                        end
                    )
                end,
                fun(X) ->
                    either:flatmap(
                        X,
                        fun
                            (_) when is_binary(ActivityDesc) ->
                                track:activity(ActivityDesc, track:activities());
                            (_) ->
                                either:left({error, <<"ActivityDesc must be a binary">>})
                        end
                    )
                end,
                fun(X) ->
                    compose:if_else(
                        fun(X2) -> ?LOG_ERROR("~p", [X2]), X2 == [] end,
                        fun(_) -> either:right(X) end,
                        fun(_) -> either:left({error, <<"There is already an active tracking">>}) end,
                        tracks:max_tracking()
                    )
                end
            ],
            []
        )
    end,
    db:transaction(F).

end_track_last() ->
    F = fun() ->
        compose:compose(
            [
                fun either_throw/1,
                %% TODO надо сделать каррирование для функций монад, можно красиво композиции писать
                fun(X) -> either:map(X, fun track:set/1) end,
                %% TODO сплитануть на 2 функции, чтобы выдавать более чёткую ошибку
                fun(X) ->
                    either:flatmap(
                        X,
                        fun(Track) ->
                            TsEnd = calendar:local_time(),
                            compose:if_else(
                                fun(_) ->
                                    track:is_timestamps_at_one_date(Track, TsEnd),
                                    track:is_timestamps_positive(Track, TsEnd)
                                end,
                                fun(_) ->
                                    either:right(track:finish(Track, TsEnd))
                                end,
                                fun(_) ->
                                    either:left({error, <<"bad timestamp">>})
                                end,
                                Track
                            )
                        end
                    )
                end,
                fun(_) ->
                    compose:if_else(
                        fun(X2) ->
                            ?LOG_ERROR("~p", [X2]),
                            X2 == []
                        end,
                        fun(_) ->
                            either:left({error, <<"There is no track to finish">>})
                        end,
                        fun either:right/1,
                        tracks:max_tracking()
                    )
                end
            ],
            []
        )
    end,
    db:transaction(F).

%% TODO надо добавить сортировку по id
%% TODO почему-то русские символы в Desc, возможно, что проблема была в моём вызове write_file, либо мнезиа как-то неправильно хранит track
export_to_csv() ->
    F = fun() ->
        compose:compose(
            [
                fun(TrackList) ->
                    lists:foldl(
                        fun
                            (CSV, <<"">>) -> CSV;
                            (CSV, Acc) -> <<Acc/binary, "\n", CSV/binary>>
                        end,
                        <<"">>,
                        [track:to_csv(Track) || Track <- TrackList]
                    )
                end,
                fun(_) -> tracks:all_sorted_by_timestamp_start() end
            ],
            []
        )
    end,
    %% можно сделать dirty-функцию
    db:transaction(F).

either_throw(Either) ->
    compose:if_else(
        fun either:is_right/1,
        fun either:extract/1,
        fun(X2) -> erlang:throw(either:extract(X2)) end,
        Either
    ).


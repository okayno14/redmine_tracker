-module(track_composed).

-include_lib("kernel/include/logger.hrl").

-export([
    begin_track/4,
    %% если последний трекинг незавершённый, то завершаем его
    end_track_last/0,
    %% экспортировать всю базу в csv
    export_to_csv/0,
    %% переписать всю базу содержимым csv
    import_from_csv/1,
    %% очистить всю базу и отправить в redmine_api
    push_to_redmine/0
]).

%% TODO проработать формат ошибок
%% Создать новый объект track в состоянии tracking с текущим системным временем начала
%% Положить в базу
-spec begin_track(
    ProjectID :: unicode:unicode_binary(),
    ActivityDesc :: unicode:unicode_binary(),
    Task :: unicode:unicode_binary(),
    Desc :: unicode:unicode_binary()
) ->
    either:either(
        {throw, {error, Msg :: unicode:unicode_binary()}}
        | {throw, track:validate_err()}
        | {Reason :: term(), erlang:stacktrace()},
        ok
    ).
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
                            (Activities) when is_binary(ActivityDesc) ->
                                track:activity(ActivityDesc, Activities);
                            (_) ->
                                either:left({error, <<"ActivityDesc must be a binary">>})
                        end
                    )
                end,
                fun(X) ->
                    either:flatmap(
                        X,
                        fun([]) ->
                            compose:if_else(
                                fun either:is_right/1,
                                fun(EitherActivities) -> either:right(either:extract(EitherActivities)) end,
                                fun(_) -> either:left({error, <<"Activities DB empty">>}) end,
                                track:activities()
                            )
                        end
                    )
                end,
                fun([]) ->
                    compose:if_else(
                        fun(X2) -> ?LOG_ERROR("~p", [X2]), X2 == [] end,
                        fun(_) -> either:right([]) end,
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
                %% TODO сплитануть на 2 функции, чтобы: если негативная разница, то юзер поправил ошибку руками, если больше дня - то сделать сплит таски
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

import_from_csv(CSV) when is_binary(CSV) ->
    F = fun() ->
        compose:compose(
            [
                fun(X) ->
                    either:map(
                        X,
                        fun(Tracks) ->
                            LastIDImported = lists:max([track:id(Track) || Track <- Tracks]),
                            LastID = track:last_id(),
                            compose:if_else(
                                fun(X2) -> X2 > 0 end,
                                fun(X2) -> track:inc_id(X2) end,
                                fun(_X2) -> ok end,
                                LastIDImported - LastID
                            )
                        end
                    )
                end,
                fun(X) ->
                    either:map(
                        X,
                        fun(Tracks) ->
                            lists:foreach(fun track:set/1, Tracks),
                            Tracks
                        end
                    )
                end,
                fun(X) ->
                    either:flatmap(
                        X,
                        fun(_Tracks) ->
                            tracks:remove_all(),
                            X
                        end
                    )
                end,
                fun(X) ->
                    either:flatmap(
                        X,
                        fun(Activities) ->
                            tracks:from_csv_all(CSV, Activities)
                        end
                    )
                end,
                fun(_) ->
                    compose:if_else(
                        fun either:is_right/1,
                        fun(EitherActivities) -> either:right(either:extract(EitherActivities)) end,
                        fun(_) -> either:left({error, <<"Activities DB empty">>}) end,
                        track:activities()
                    )
                end
            ],
            []
        )
    end,
    db:transaction(F).

push_to_redmine() ->
    Param =
        fun(Config, Key) ->
            case proplists:get_value(Key, Config, not_found) of
                not_found -> either:left({error, not_found});
                V -> either:right(V)
            end
        end,

    UpdateParam =
            fun(Either, Key) ->
                either:flatmap(
                    Either,
                    fun(X) ->
                        #{config := Config} = X,
                        ?LOG_DEBUG("X:~p Param:~p", [X, Param(Config, Key)]),
                        compose:if_else(
                            fun either:is_right/1,
                            fun(X2) -> either:right(maps:put(Key, either:extract(X2), X)) end,
                            fun(_) -> either:left({error, {not_configured, Key}}) end,
                            Param(Config, Key)
                        )
                    end
                )
            end,

    compose:compose(
        [
            fun(Either) ->
                either:flatmap(
                    Either,
                    fun(Tracks) ->
                        db:transaction(fun() -> [track:delete(Track) || Track <- Tracks] end)
                    end
                )
            end,
            fun(Either) ->
                either:flatmap(
                    Either,
                    fun(X) ->
                        #{
                            user_id := UserId,
                            redmine_instance := RedmineInstance,
                            api_key := ApiKey,
                            tracks := Tracks
                        } = X,
                        Ret =
                            lists:foldl(
                                fun(Track, ErrorList) ->
                                    case track:push_to_redmine(Track, UserId, RedmineInstance, ApiKey) of
                                        ok -> ErrorList;
                                        {error, Reason} -> [{track:id(Track), Reason} | ErrorList]
                                    end
                                end,
                                [],
                                Tracks
                            ),
                        case Ret of
                            [] -> either:right(Tracks);
                            _ -> either:left({error, {push, Ret}})
                        end
                    end
                )
            end,
            %% TODO можно переписать: параметры всегда лежат в конфиге, дефолт настроен -> можно написать проще
            fun(Either) -> UpdateParam(Either, api_key) end,
            fun(Either) -> UpdateParam(Either, redmine_instance) end,
            fun(Either) -> UpdateParam(Either, user_id) end,
            fun(Either) ->
                either:map(
                    Either,
                    fun(X) ->
                        %% TODO вынести в модуль Config
                        X#{config => application:get_all_env(redmine_tracker)}
                    end
                )
            end,
            fun(X) ->
                either:cata(
                    db:transaction(fun tracks:all/0),
                    fun(Reason) -> {error, {db_fetch, Reason}} end,
                    fun(Tracks) -> X#{tracks => Tracks} end
                )
            end
        ],
        #{}
    ).

%% @doc For aborting db transaction
% -spec either_throw(Either :: either:either(dynamic(), V)) ->
%     V.
either_throw(Either) ->
    compose:if_else(
        fun either:is_right/1,
        fun either:extract/1,
        fun(X2) -> erlang:throw(either:extract(X2)) end,
        Either
    ).


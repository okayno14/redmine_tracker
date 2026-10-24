-module(track_composed).

-include_lib("kernel/include/logger.hrl").

-export([
    begin_track/4,
    %% если последний трекинг незавершённый, то завершаем его
    end_track_last/0,
    %% экспортировать всю базу в csv
    export/1,
    %% переписать всю базу содержимым csv
    import/2,
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
    db:transaction_ret(
        {error, Msg :: unicode:unicode_binary()} | track:validate_err(),
        ok
    ).
begin_track(ProjectID, ActivityDesc, Task, Desc) ->
    F = fun() ->
        compose:compose(
            [
                fun db:either_throw/1,
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
                        fun(Activities) ->
                            either:leftmap(
                                track:activity(ActivityDesc, Activities),
                                fun(_) ->
                                    {
                                        error,
                                        unicode:characters_to_binary(
                                            [
                                                ~"No such Activity:", ActivityDesc,
                                                ~" in Activities-db"
                                            ]
                                        )
                                    }
                                end
                            )
                        end
                    )
                end,
                fun(X) ->
                    either:flatmap(
                        X,
                        fun
                            (Activities) when is_binary(ActivityDesc) ->
                                X;
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

-spec end_track_last() ->
    db:transaction_ret(
        {error, Msg :: unicode:unicode_binary()},
        ok
    ).
end_track_last() ->
    F = fun() ->
        compose:compose(
            [
                fun db:either_throw/1,
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
                                    track:is_timestamps_at_one_date(Track, TsEnd) andalso
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

%%--------------------------------------------------------------------
%% TODO надо добавить сортировку по id
-spec export(Format :: unicode:unicode_binary()) ->
    db:transaction_ret(CSV :: unicode:unicode_binary()).
%%--------------------------------------------------------------------
%% TODO remove csv from method names. Cause it can be any format: json, whatever
export(Format) when is_binary(Format) ->
    db:transaction(
        fun() ->
            compose:compose(
                [
                    fun db:either_throw/1,
                    fun export_process_ret/1,
                    fun maps_to_csv_/1,
                    fun tracks_to_maps/1,
                    fun tracks_all/1,
                    fun(X) -> track_translator(X, Format) end
                ],
                either:right(
                    #{
                        track_translator => undefined,
                        csv => [],
                        timestamp => {{0, 0, 0}, {0, 0, 0}}
                    }
                )
            )
         end
     ).

export_process_ret(X) ->
    either:cata(
        X,
        fun({error, {unknown_format, Format}}) ->
            Msg =
                unicode:characters_to_binary([
                    <<"No translator for ">>,
                    <<"\"">>, Format, <<"\"">>, <<" ">>,
                    <<"format.">>
                ]),
            {error, Msg}
        end,
        fun(#{csv := CSV}) ->
            CSV
        end
    ).

maps_to_csv_(X) ->
    either:map(
        X,
        fun(Args) ->
            #{csv := Tracks, track_translator := TrackTranslator} = Args,
            Args#{csv => track_translator:export(TrackTranslator, Tracks)}
        end
    ).

tracks_to_maps(X) ->
    either:map(
        X,
        fun(Args) ->
            #{csv := Tracks} = Args,
            Args#{csv => [track:to_map(Track) || Track <- Tracks]}
        end
    ).

tracks_all(X) ->
    either:map(
        X,
        fun(Args) ->
            Args#{csv => tracks:all_sorted_by_timestamp_start()}
        end
    ).

track_translator(X, Format) ->
    either:flatmap(
        X,
        fun(Args) ->
            either:cata(
                track_translator_strategy:make(Format),
                fun({error, unknown_format}) -> {error, {unknown_format, Format}} end,
                %% TODO переименовать
                fun(TrackTranslator) -> Args#{track_translator => TrackTranslator} end
            )
        end
    ).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
-spec import(
    CSV :: unicode:unicode_binary(), Format :: unicode:unicode_binary()
) ->
    db:transaction_ret(
        tracks:from_csv_all_err()
        | {error, Msg :: unicode:unicode_binary()},
        ok
    ).
%%--------------------------------------------------------------------
import(CSV, Format) when is_binary(CSV), is_binary(Format) ->
    db:transaction(
        fun() ->
            compose:compose(
                [
                    %% TODO добавить вызов track:validate, т.к. мало ли что там юзер прислал.
                    %% TrackTranslator возвращает только ошибки конвертации формата, он не проверяет сам объект Track.
                    %% TODO добавить перегенерацию обратного csv из export (чтобы было разделение по датам)
                    fun db:either_throw/1,
                    fun import_from_csv_process_ret/1,
                    fun maybe_inc_id/1,
                    fun tracks_set/1,
                    fun remove_all/1,
                    fun tracks_from_map/1,
                    fun import_tracks/1,
                    fun(X) -> import_from_csv_track_translator(X, Format) end
                ],
                either:right(CSV)
            )
        end
    ).

%% TODO надо удалить, т.к. эта логика делается на стороне контроллера
import_from_csv_process_ret(X) ->
    either:leftmap(
        X,
        fun({error, {unknown_format, Format}}) ->
            %% TODO вынести в отдельную функцию
            Msg =
                unicode:characters_to_binary([
                    <<"No translator for ">>,
                    <<"\"">>, Format, <<"\"">>, <<" ">>,
                    <<"format.">>
                ]),
            {error, Msg}
        end
    ).

maybe_inc_id(X) ->
    either:map(
        X,
        fun(Tracks) ->
            LastIDImported = lists:max([track:id(Track) || Track <- Tracks]),
            LastID = track:last_id(),
            compose:if_else(
                fun(X2) -> X2 > 0 end,
                fun(X2) -> track:inc_id(X2), ok end,
                fun(_X2) -> ok end,
                LastIDImported - LastID
            )
        end
    ).

tracks_set(X) ->
    either:map(X, fun(Tracks) -> lists:foreach(fun track:set/1, Tracks), Tracks end).

remove_all(X) ->
    either:flatmap(X, fun(_Tracks) -> tracks:remove_all(), X end).

tracks_from_map(X) ->
    either:map(X, fun(L) -> lists:map(fun track:from_map/1, L) end).

import_tracks(X) ->
    either:flatmap(
        X,
        fun({TrackTranslator, CSV}) ->
            track_translator:import(TrackTranslator, CSV)
        end
    ).

import_from_csv_track_translator(X, Format) ->
    either:flatmap(
        X,
        fun(CSV) ->
            either:cata(
                track_translator_strategy:make(Format),
                fun({error, unknown_format}) -> {error, {unknown_format, Format}} end,
                fun(TrackTranslator) -> {TrackTranslator, CSV} end
            )
        end
    ).
%%--------------------------------------------------------------------

-spec push_to_redmine() ->
    db:transaction_ret(
        %% TODO change to track_id()
        {error, [{Id :: pos_integer(), track:push_to_redmine_reason()}]}
        | {error, {not_configured, Key :: atom()}},
        ok
    ).
push_to_redmine() ->
    db:transaction(fun push_to_redmine_/0).

push_to_redmine_() ->
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
            fun db:either_throw/1,
            fun(Either) ->
                either:flatmap(
                    Either,
                    fun(X) ->
                        #{tracks := Tracks, error_list := ErrorList} = X,
                        [track:delete(Track) || Track <- Tracks],
                        case ErrorList of
                            [] -> either:right(ok);
                            _ -> either:left(ErrorList)
                        end
                    end
                )
            end,
            fun(Either) ->
                either:map(
                    Either,
                    fun(X) ->
                        #{
                            user_id := UserId,
                            redmine_instance := RedmineInstance,
                            api_key := ApiKey,
                            tracks := Tracks
                        } = X,
                        {Tracks3, ErrorList2} =
                            lists:foldl(
                                fun(Track, {Tracks2, ErrorList}) ->
                                    case track:push_to_redmine(Track, UserId, RedmineInstance, ApiKey) of
                                        ok -> {[Track | Tracks2], ErrorList};
                                        {error, Reason} -> {Tracks2, [{track:id(Track), Reason} | ErrorList]}
                                    end
                                end,
                                {[], []},
                                Tracks
                            ),
                        X#{tracks => Tracks3, error_list => ErrorList2}
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
            fun(X) -> either:right(X#{tracks => tracks:all()}) end
        ],
        #{}
    ).


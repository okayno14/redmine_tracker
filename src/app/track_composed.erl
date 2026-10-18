-module(track_composed).

-include_lib("kernel/include/logger.hrl").

-export([
    begin_track/4,
    %% если последний трекинг незавершённый, то завершаем его
    end_track_last/0,
    %% экспортировать всю базу в csv
    export_to_csv/1,
    %% переписать всю базу содержимым csv
    import_from_csv/2,
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
-spec export_to_csv(Format :: unicode:unicode_binary()) ->
    db:transaction_ret(CSV :: unicode:unicode_binary()).
%%--------------------------------------------------------------------
export_to_csv(Format) when is_binary(Format) ->
    db:transaction(fun() -> export_to_csv_(Format) end).

export_to_csv_(Format) ->
    compose:compose(
        [
            fun db:either_throw/1,
            fun(X) ->
                either:cata(
                    X,
                    fun({error, unknown_format}) ->
                        Msg =
                            unicode:characters_to_binary([
                                <<"No translator for ">>,
                                <<"\"">>, Format, <<"\"">>, <<" ">>,
                                <<"format.">>
                            ]),
                        {error, Msg}
                    end,
                    fun(Y) -> Y end
                )
            end,
            fun(X) ->
                either:map(
                    X,
                    fun(Args) ->
                        #{csv := Tracks} = Args,
                        unicode:characters_to_binary(Tracks)
                    end
                )
            end,
            fun(X) ->
                either:map(
                    X,
                    fun
                         (Args = #{csv := []}) ->
                             Args;
                         (Args) ->
                             #{csv := Tracks} = Args,
                             [_ | T] = lists:reverse(Tracks),
                             Args#{csv => T}
                     end
                )
            end,
            fun(X) ->
                either:map(
                    X,
                    fun tracks_to_csv/1
                )
            end,
            fun(X) ->
                either:map(
                    X,
                    fun
                        (Args = #{csv := []}) -> Args;
                        (Args = #{csv := Tracks}) -> Args#{timestamp => track:ts_begin(erlang:hd(Tracks))}
                    end
                )
            end,
            fun(X) ->
                either:map(
                    X,
                    fun(Args) ->
                        Args#{csv => tracks:all_sorted_by_timestamp_start()}
                    end
                )
            end,
            fun(X) ->
                either:map(
                    X,
                    fun(TrackTranslator) ->
                        #{
                            to_csv => fun(Track) -> track_translator:export(TrackTranslator, [track:to_map(Track)]) end,
                            csv => [],
                            timestamp => {{0, 0, 0}, {0, 0, 0}}
                        }
                    end
                )
            end
        ],
        track_translator_strategy:make(Format)
    ).

tracks_to_csv(X = #{csv := []}) ->
    X;
tracks_to_csv(X) ->
    #{
        csv := Tracks,
        to_csv := ToCsv
    } = X,
    lists:foldl(
        fun(
            Track,
            #{csv := CSV, timestamp := TS}
        ) when is_list(CSV) ->
            compose:if_else(
                fun(X2) ->
                    track:is_timestamps_at_one_date(X2, TS)
                end,
                fun(X2) ->
                    #{
                        csv => [ToCsv(X2), ~"\n" | CSV],
                        timestamp => track:ts_begin(X2)
                    }
                end,
                fun(X2) ->
                    #{
                        csv => [ToCsv(X2), ~"\n", ~"\n" | CSV],
                        timestamp => track:ts_begin(X2)
                    }
                end,
                Track
            )
        end,
        X#{csv => []},
        Tracks
    ).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
-spec import_from_csv(
    CSV :: unicode:unicode_binary(), Format :: unicode:unicode_binary()
) ->
    db:transaction_ret(
        tracks:from_csv_all_err()
        | {error, Msg :: unicode:unicode_binary()},
        ok
    ).
%%--------------------------------------------------------------------
import_from_csv(CSV, Format) when is_binary(CSV), is_binary(Format) ->
    F = fun() ->
        compose:compose(
            [
                fun db:either_throw/1,
                fun(X) ->
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
                    either:map(
                        X,
                        fun(L) ->
                            lists:map(fun track:from_map/1, L)
                        end
                    )
                end,
                fun(X) ->
                    either:flatmap(
                        X,
                        fun({TrackTranslator, CSV2}) ->
                            track_translator:import(TrackTranslator, CSV2)
                        end
                    )
                end,
                fun(X) ->
                    either:cata(
                        track_translator_strategy:make(Format),
                        fun({error, unknown_format}) ->
                            Msg =
                                unicode:characters_to_binary([
                                    <<"No translator for ">>,
                                    <<"\"">>, Format, <<"\"">>, <<" ">>,
                                    <<"format.">>
                                ]),
                            {error, Msg}
                        end,
                        fun(TrackTranslator) ->
                            {TrackTranslator, CSV}
                        end
                    )
                end
            ],
            CSV
        )
    end,
    db:transaction(F).
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


-module(track_composed).

-include_lib("kernel/include/logger.hrl").

-export([
    begin_track/4
    %% если последний трекинг незавершённый, то завершаем его
    % end_track_last
    %% экспортировать всю базу в csv
    % export_to_csv
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
    %% TODO по-моему транзакция должна жить внутри контроллера?
    %% TODO а точно ли валидация должна быть тут, а не в контроллере?
    db:transaction(F).

either_throw(Either) ->
    compose:if_else(
        fun either:is_right/1,
        fun either:extract/1,
        fun(X2) -> erlang:throw(either:extract(X2)) end,
        Either
    ).


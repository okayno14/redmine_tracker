-module(redmine_tracker_controller).

-behaviour(controller).

-include_lib("kernel/include/logger.hrl").

-export([route/1]).

route(#{request := <<"export_to_csv">>}) ->
    %% TODO надо отлавливать ошибку транзакции
    {atomic, CSV} = track_composed:export_to_csv(),
    response:ok_response(CSV);
route(_UnknownReq) ->
    ?LOG_ERROR("Unkown Req:~p", [_UnknownReq]),
    response:ok_response(<<"ok">>).


-module(redmine_tracker_controller).

-include_lib("kernel/include/logger.hrl").

-behaviour(controller_2).

-export([route/1]).

route(#{request := <<"export_to_csv">>}) ->
    {
        fun() ->
            track_composed:export_to_csv()
        end,
        fun(Either) ->
            case {either:is_right(Either), either:extract(Either)} of
                {true, CSV} -> response:ok_response(CSV);
                _ -> nomatch
            end
        end
    };
route(_UnknownReq) ->
    ?LOG_ERROR("Unkown Req:~p", [_UnknownReq]),
    {
        fun(_) -> ok end,
        fun(_) -> response:error_response(no_route, <<"Unknown request">>) end
    }.


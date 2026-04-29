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
route(#{request := <<"import_from_csv">>, <<"csv">> := CSV}) ->
    {
        fun() ->
            track_composed:import_from_csv(CSV)
        end,
        fun(Either) ->
            case {either:is_right(Either), either:extract(Either)} of
                {true, ok} ->
                    response:ok_response(CSV);
                {false, {throw, {error, Msg}}} when is_binary(Msg) ->
                    response:error_response(bad_import, Msg);
                {false, {throw, {error, {bad_csv, Line}, Error}}} ->
                    response:error_response(
                        bad_csv,
                        unicode:characters_to_binary(
                            io_lib:format("Bad csv-line: ~ts\nReason:~p", [Line, Error])
                        )
                    );
                {false, {throw, {error, bad_csv}}} ->
                    reponse:error_response(bad_csv, <<"unknown csv-error">>);
                _V ->
                    nomatch
            end
        end
    };
route(_UnknownReq) ->
    ?LOG_ERROR("Unkown Req:~p", [_UnknownReq]),
    {
        fun(_) -> ok end,
        fun(_) -> response:error_response(no_route, <<"Unknown request">>) end
    }.


-module(redmine_tracker_controller).

-behaviour(controller).

-include_lib("kernel/include/logger.hrl").

-export([route/1]).

-eqwalizer({nowarn_function, process_transaction/2}).

route(#{request := <<"export_to_csv">>}) ->
    process_transaction(
        fun() ->
            track_composed:export_to_csv()
        end,
        fun
            ({atomic, CSV}) ->
                response:ok_response(CSV);
            (_) ->
                nomatch
        end
    );
route(_UnknownReq) ->
    ?LOG_ERROR("Unkown Req:~p", [_UnknownReq]),
    response:ok_response(<<"ok">>).

%% TODO move to inner controller code after type specification of track_composed
process_transaction(Transaction, HappyPath) ->
    ErrorResponse = fun
        ({aborted, {Reason, ST}}) when is_list(ST) ->
            response:error_response(
                transaction_failed,
                unicode:characters_to_binary(
                    erl_error:format_exception(error, Reason, ST)
                )
            );
        ({aborted, Reason}) ->
            response:error_response(
                transaction_failed,
                unicode:characters_to_binary(io_lib:format("~p", [Reason]))
            )
    end,
    Ret = Transaction(),
    case HappyPath(Ret) of
        nomatch -> ErrorResponse(Ret);
        #{type := ok} = OK -> OK
    end.


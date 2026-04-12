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
        fun(Either) ->
            case {either:is_right(Either), either:extract(Either)} of
                {true, CSV} -> response:ok_response(CSV);
                _ -> nomatch
            end
        end
    );
route(_UnknownReq) ->
    ?LOG_ERROR("Unkown Req:~p", [_UnknownReq]),
    response:ok_response(<<"ok">>).

%% TODO change spec for controller behaviour. Make controller-decorator (function in controller-module), that uses new controller (returns funs) and transforms it to responses
process_transaction(Transaction, HappyPath) ->
    ErrorResponse = fun(Either) ->
        case either:extract(Either) of
            {Reason, ST} when is_list(ST) ->
                response:error_response(
                    transaction_failed,
                    unicode:characters_to_binary(
                        erl_error:format_exception(error, Reason, ST)
                    )
                );
            {throw, Reason} ->
                response:error_response(
                    transaction_failed,
                    unicode:characters_to_binary(io_lib:format("~p", [Reason]))
                )
        end
    end,
    Ret = Transaction(),
    case HappyPath(Ret) of
        nomatch -> ErrorResponse(Ret);
        #{type := ok} = OK -> OK
    end.


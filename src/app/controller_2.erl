%% @doc Controller implementation, that relyes on db:transaction/1
-module(controller_2).

-export([route/1]).

-behaviour(controller).

-callback route(request:request()) ->
    {
        Transaction :: fun(() -> either:either(Error, Res)),
        %% Either - db-transaction status
        HappyPath :: fun(
            (Either :: either:either(Error, Res)) -> response:response() | nomatch
        )
    }.

-spec route(Request :: request:request()) ->
    response:response().
route(Request) ->
    {Transaction, HappyPath} = redmine_tracker_controller:route(Request),
    DefaultHandler =
        fun(Either) ->
            case {either:is_left(Either), either:extract(Either)} of
                {true, {Reason, ST}} when is_list(ST) ->
                    response:error_response(
                        transaction_failed,
                        unicode:characters_to_binary(
                            erl_error:format_exception(error, Reason, ST)
                        )
                    );
                {true, {throw, Reason}} ->
                    response:error_response(
                        transaction_failed,
                        unicode:characters_to_binary(io_lib:format("~p", [Reason]))
                    );
                {true, Reason} ->
                    response:error_response(
                        transaction_failed,
                        unicode:characters_to_binary(io_lib:format("~p", [Reason]))
                    );
                {false, X} ->
                    response:ok_response(io_lib:format("~p", [X]))
            end
        end,
    Ret = Transaction(),
    case HappyPath(Ret) of
        nomatch -> DefaultHandler(Ret);
        #{type := ok} = OK -> OK;
        #{type := error} = Error -> Error
    end.


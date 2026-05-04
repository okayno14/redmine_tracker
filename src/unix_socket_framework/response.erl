-module(response).

-export([
    ok_response/1,
    error_response/2,
    encode/2,
    decode/1,
    format/1
]).

-export_type([
    response/0,
    ok_response/0,
    error_response/0,
    error_response/1
]).

-type response() :: ok_response() | error_response().

%% No correlation headers, cause protocol works over TCP
-type ok_response() ::
    #{
        type := ok,
        data := json:encode_value()
    }.

%% No error codes, cause client must display error text to user
-type error_response() ::
    #{
        type := error,
        reason := atom(),
        msg := unicode:unicode_binary()
    }.

-type error_response(Reason) ::
    #{
        type := error,
        reason := Reason,
        msg := unicode:unicode_binary()
    }.

-spec ok_response(Data :: json:encode_value()) ->
    ok_response().
ok_response(Data) ->
    #{type => ok, data => Data}.

-spec error_response(Reason, Msg :: unicode:unicode_binary()) ->
    error_response(Reason)
when
    Reason :: atom().
error_response(Reason, Msg) ->
    #{
        type => error,
        reason => Reason,
        msg => Msg
    }.

-spec encode(Response :: response(), oneline | indent) ->
    JSON :: unicode:unicode_binary().
encode(Response, oneline) ->
    true = is_response(Response),
    json2:encode(Response, oneline);
encode(Response, indent) ->
    true = is_response(Response),
    json2:encode(Response, indent).

%%--------------------------------------------------------------------
-spec decode(Binary :: unicode:unicode_binary()) ->
    either:either(
        {error, json2:decode_reason()},
        error_response() | ok_response()
    ).
decode(Binary) ->
    compose:compose(
        [
            fun(Either) ->
                either:flatmap(
                    Either,
                    fun
                        (
                            #{
                                <<"type">> := <<"error">>,
                                <<"reason">> := Reason,
                                <<"msg">> := Msg
                            }
                        ) when is_binary(Reason), is_binary(Msg) ->
                            either:right(
                                #{
                                    type => error,
                                    reason => erlang:binary_to_atom(Reason),
                                    msg => Msg
                                }
                            );
                        (
                            #{
                                <<"type">> := <<"ok">>,
                                <<"data">> := Data
                            }
                        ) ->
                            either:right(
                                #{
                                    type => ok,
                                    data => Data
                                }
                            );
                        (_) ->
                            either:left({error, bad_response})
                    end
                )
            end,
            fun(X) -> json2:decode(X) end
        ],
        Binary
    ).
%%--------------------------------------------------------------------

-spec format(response()) ->
    unicode:unicode_binary().
format(#{
    type := ok,
    data := Data
}) when is_binary(Data) ->
    X = unicode:characters_to_binary(io_lib:format("~ts", [Data])),
    true = is_binary(X),
    X;
format(#{
    type := ok,
    data := Data
}) ->
    X = unicode:characters_to_binary(io_lib:format("~p", [Data])),
    true = is_binary(X),
    X;
format(#{
    type := error,
    reason := Reason,
    msg := Msg
}) ->
    X =
        unicode:characters_to_binary(
            io_lib:format("reason: ~ts\nmsg: ~ts", [Reason, Msg])
        ),
    true = is_binary(X),
    X.

is_response(#{type := ok, data := _}) ->
    true;
is_response(#{type := error, reason := Reason, msg := _}) when is_atom(Reason) ->
    true;
is_response(_) ->
    false.


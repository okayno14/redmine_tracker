-module(response).

-export([
    ok_response/1,
    error_response/2
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


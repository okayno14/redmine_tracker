-module(response).

-export([
    ok_response/1,
    error_response/1
]).

-export_type([
    response/0,
    ok_response/0,
    error_response/0
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
        msg := unicode:unicode_binary()
    }.

-spec ok_response(Data :: json:encode_value()) ->
    ok_response().
ok_response(Data) ->
    #{type => ok, data => Data}.

-spec error_response(Msg :: unicode:unicode_binary()) ->
    error_response().
error_response(Msg) ->
    #{type => error, msg => Msg}.


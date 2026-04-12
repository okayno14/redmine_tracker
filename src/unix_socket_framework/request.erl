-module(request).

-export([decode/1]).

-export_type([request/0]).

-type request() ::
    #{
        request := unicode:unicode_binary(),
        binary() => json:decode_value()
    }.

%% @doc Uses json, can generate erlang:error()
decode(Binary) ->
    maybe
        Y = #{} ?= json:decode(Binary),
        {V, Y2} ?= maps:take(<<"request">>, Y),
        Y2#{request => V}
    else
        _ -> {error, not_request}
    end.


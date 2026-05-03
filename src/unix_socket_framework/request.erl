-module(request).

-export([decode/1]).

-export_type([request/0]).

-type request() ::
    #{
        request := unicode:unicode_binary(),
        binary() => json:decode_value()
    }.

-spec decode(Binary :: unicode:unicode_binary()) ->
    either:either(
        {error, not_request} | {error, json2:decode_reason()},
        request()
    ).
decode(Binary) ->
    compose:compose(
        [
            fun(Either) ->
                either:flatmap(
                    Either,
                    fun(X) ->
                        case maps:take(~"request", X) of
                            error -> either:left({error, not_request});
                            {V, X2} -> either:right(X2#{request => V})
                        end
                    end
                )
            end,
            fun(Binary2) -> json2:decode(Binary2) end
        ],
        Binary
    ).


-module(request).

-export([
    encode/2,
    decode/1,
    format/1
]).

-export_type([request/0]).

-eqwalizer({nowarn_function, format/1}).

-type request() ::
    #{
        request := unicode:unicode_binary(),
        binary() => json:decode_value()
    }.

-spec encode(Binary :: request(), oneline | indent) ->
    JSON :: unicode:unicode_binary().
encode(Request = #{request := _}, oneline) ->
    json2:encode(Request, oneline);
encode(Request = #{request := _}, indent) ->
    json2:encode(Request, indent).

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

-spec format(Request :: request()) ->
    unicode:unicode_binary().
format(Request = #{request := _}) ->
    unicode:characters_to_binary(
        lists:flatmap(
            fun
                ({K, V}) when is_binary(V) -> ["\n---\n", io_lib:format("~p:\n~ts", [K, V])];
                ({K, V}) -> ["\n---\n", io_lib:format("~p:\n~p", [K, V])]
            end,
            maps:to_list(Request)
        ) ++ ["\n---"]
    ).


-module(json2).

-export([
    encode/2,
    decode/1
]).

-export_type([
    decode_reason/0
]).

-spec encode(json:encode_value(), Format :: oneline | indent) ->
    unicode:unicode_binary().
encode(Term, indent) ->
    Bin = unicode:characters_to_binary(json:format(Term)),
    true = is_binary(Bin),
    Bin;
encode(Term, oneline) ->
    Bin = unicode:characters_to_binary(json:encode(Term)),
    true = is_binary(Bin),
    Bin.

-type decode_reason() ::
    bad_response
    | {invalid_byte, Byte :: byte()}
    | {unexpected_sequence, Bytes :: binary()}.

-spec decode(Binary :: unicode:unicode_binary()) ->
    either:either(
        {error, decode_reason()},
        json:decode_value()
    ).
decode(Binary) ->
    compose:compose(
        [
            fun(Either) ->
                % in documentation said, that we must expect this kind of errors,
                % for type safety we must check returned error
                either:leftmap(
                    Either,
                    fun
                        ({error, unexpected_end, _StackTrace}) ->
                            {error, unexpected_end};
                        ({error, {invalid_byte, Byte}, _StackTrace}) ->
                            {error, {invalid_byte, Byte}};
                        ({error, {unexpected_sequence, _Bytes}, _StackTrace}) ->
                            {error, {unexpected_sequence, _Bytes}}
                    end
                )
            end,
            fun(X) -> either:from_try(fun() -> json:decode(X) end) end
        ],
        Binary
    ).


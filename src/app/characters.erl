-module(characters).

-export([
    to_list/1,
    to_binary/1
]).

-export_type([characters/0]).

-type characters() ::
    unicode:latin1_chardata()
    | unicode:chardata()
    | unicode:external_chardata().

to_list(Chars) ->
    Ret = unicode:characters_to_list(Chars),
    true = erlang:is_list(Ret),
    Ret.

to_binary(Chars) ->
    Ret = unicode:characters_to_binary(Chars),
    true = erlang:is_binary(Ret),
    Ret.


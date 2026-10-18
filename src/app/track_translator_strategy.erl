-module(track_translator_strategy).

-export([make/1]).

-spec make(Format :: unicode:unicode_binary()) ->
    either:either(
        {error, unknown_format},
        track_translator:track_translator()
    ).
make(<<"csv">>) ->
    either:right(
        #{
            import => fun track_csv:import/1,
            export => fun track_csv:export/1
        }
    );
make(_) ->
    either:left({error, unknown_format}).




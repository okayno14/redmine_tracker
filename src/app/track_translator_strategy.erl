-module(track_translator_strategy).

-export([make/1]).

make(<<"csv">>) ->
    #{
        import => fun track_csv:import/1,
        export => fun track_csv:export/1
    }.


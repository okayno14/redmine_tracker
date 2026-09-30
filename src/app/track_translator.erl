-module(track_translator).

-export([export/2, import/2]).

-export_type([track_translator/0]).

-type track_translator() ::
    #{
        import := fun(
            (unicode:unicode_binary()) ->
                either:either(
                    {error, Reason :: unicode:unicode_binary()},
                    [track:external_map()]
                )
        ),
        export := fun(([track:external_map()]) -> unicode:unicode_binary())
    }.

-callback export([track:external_map()]) ->
    Bin :: unicode:unicode_binary().

-callback import(Bin :: unicode:unicode_binary()) ->
    either:either(
        {error, Reason :: unicode:unicode_binary()},
        [track:external_map()]
    ).

%%--------------------------------------------------------------------
-spec export(track_translator(), [track:external_map()]) ->
    Bin :: unicode:unicode_binary().
%%--------------------------------------------------------------------
export(TrackTranslator, L) ->
    #{export := F} = TrackTranslator,
    F(L).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
-spec import(track_translator(), Bin :: unicode:unicode_binary()) ->
    either:either(
        {error, Reason :: unicode:unicode_binary()},
        [track:external_map()]
    ).
%%--------------------------------------------------------------------
import(TrackTranslator, Bin) ->
    #{import := F} = TrackTranslator,
    F(Bin).
%%--------------------------------------------------------------------


-module(tracks).

-include_lib("redmine_tracker/include/track.hrl").

%% clean functions
-export([
    from_csv_all/2
]).

%% dirty functions
-export([
    all/0
]).

-eqwalizer({nowarn_function, from_csv_all/2}).
-eqwalizer({nowarn_function, all/0}).

%%--------------------------------------------------------------------
-spec from_csv_all(CSV :: unicode:unicode_binary(), Activities :: activites()) ->
    either:either(
        {error, bad_csv}
        | {error, {bad_csv, Line :: unicode:unicode_binary()}, track:from_csv_err()},
        track:track()
    ).
%%--------------------------------------------------------------------
from_csv_all(CSV, Activities) ->
    lists:foldl(
        fun
            (<<>>, Either) ->
                either:map(Either, fun lists:reverse/1);
            (Line, Either) when is_binary(Line) ->
                either:flatmap(
                    Either,
                    fun
                        (Acc) ->
                            Ret = track:from_csv(Line, Activities),
                            case either:is_left(Ret) of
                                true ->
                                    either:left(
                                        {error,
                                            {bad_csv, Line},
                                            either:extract(Ret)
                                        }
                                    );
                                false ->
                                    either:right(
                                        [either:extract(Ret) | Acc]
                                    )
                            end
                    end
                )
        end,
        either:right([]),
        string:split(CSV, <<"\n">>, all)
    ).
%%--------------------------------------------------------------------

all() ->
    mnesia:match_object(#track{_ = '_'}).


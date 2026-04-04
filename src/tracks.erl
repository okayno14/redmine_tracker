-module(tracks).

-include_lib("kernel/include/logger.hrl").
-include_lib("redmine_tracker/include/track.hrl").

%% clean functions
-export([
    from_csv_all/2
]).

%% dirty functions
-export([
    all/0,
    max_tracking/0
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

%% TODO может запихать в track? Т.к. жёстко нарушаем инкапсуляцию track
%% TODO может использовать какую-нибудь proplists, key* - функцию?
%% TODO надо по времени сравнивать, а не id
max_tracking() ->
    L = mnesia:match_object(#track{state = tracking, _ = '_'}),
    ?LOG_ERROR("~p", [L]),
    list_max(
        fun(X, Acc) ->
            %% TODO спрятать в track?
            A = X#track.id,
            B = Acc#track.id,
            case {A, B} of
                {A, A} -> eq;
                {B, B} -> eq;
                {A, B} when A > B -> gt;
                _ -> lt
            end
        end,
        L
    ).
list_max(F, []) -> [];
list_max(F, L) -> list_max_(F, L, erlang:hd(L)).

list_max_(F, [], Acc) ->
    Acc;
list_max_(F, [H | T], Acc) ->
    case F(H, Acc) of
        gt -> list_max_(F, T, H);
        eq -> list_max_(F, T, H);
        lt -> list_max_(F, T, Acc)
    end.


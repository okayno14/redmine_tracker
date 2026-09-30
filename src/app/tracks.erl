-module(tracks).

-include_lib("kernel/include/logger.hrl").
-include_lib("redmine_tracker/include/track.hrl").

%% dirty functions
-export([
    all_sorted_by_timestamp_start/0,
    all_sorted_by_id/0,
    all/0,
    max_tracking/0,
    remove_all/0
]).

-eqwalizer({nowarn_function, from_csv_all/2}).
-eqwalizer({nowarn_function, all/0}).

%%--------------------------------------------------------------------
%% TODO переименовать, надо ли внурь track запихать функцию
all_sorted_by_timestamp_start() ->
    lists:sort(
        fun(TrackA, TrackB) ->
            track:is_timestamps_positive(TrackA, TrackB#track.timestamp_begin)
        end,
        all()
    ).

all_sorted_by_id() ->
    lists:keysort(#track.id, all()).

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

remove_all() ->
    [mnesia:delete({track, Track#track.id}) || Track <- all()],
    ok.

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


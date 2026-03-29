%% NO INCLUDE IN EXTERNAL FILES
%% this hrl used for modules track, tracks

-record(track, {
    id :: pos_integer(),
    project_id :: unicode:unicode_binary(),
    activity :: activity(),
    state :: track_state(),
    task :: unicode:unicode_binary(),
    timestamp_begin :: calendar:datetime(),
    timestamp_end :: calendar:datetime(),
    desc :: unicode:unicode_binary()
}).

-record(track_id, {key = key, last_id = 0 :: pos_integer()}).

-type track_state() :: tracking | finished.

-type activites() ::
    #{
        ActivityDesc :: unicode:unicode_binary() => ActivityID :: pos_integer()
    }.

-type activity() :: {
    ActivityID :: pos_integer(), Activity :: unicode:unicode_binary()
}.


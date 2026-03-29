-module(tracks).

-include_lib("redmine_tracker/include/track.hrl").

-export([
    all/0
]).

-eqwalizer({nowarn_function, all/0}).
all() ->
    mnesia:match_object(#track{_ = '_'}).


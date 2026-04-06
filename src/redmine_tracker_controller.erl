-module(redmine_tracker_controller).

-behaviour(controller).

-export([route/1]).

route(_) ->
    response:ok_response(<<"ok">>).


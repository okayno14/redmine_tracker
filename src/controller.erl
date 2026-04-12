-module(controller).

-export([route/2]).

-callback route(Request :: request:request()) ->
    response:response().

-spec route(Mod :: module(), Request :: request:request()) ->
    response:response().
route(Mod, Request) ->
    Mod:route(Request).


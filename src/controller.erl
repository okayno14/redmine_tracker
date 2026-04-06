-module(controller).

-callback route(Request :: request:request()) ->
    response:response().


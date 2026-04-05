-module(socket_sup).
-behaviour(supervisor).

%% API
-export([
    start_link/0
]).

%% Callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 1,
        period => 5
    },
    {ok, Socket} = application:get_env(socket),
    ok = file:delete(Socket),
    {ok, ListenSocket} =
        gen_tcp:listen(
            0,
            [
                {ifaddr, {local, Socket}},
                {mode, binary}
            ]
        ),
    ChildSpecs = child_specs(ListenSocket),
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

child_specs(ListenSocket) ->
    ChildSpec =
        fun(Id, ListenSocket2) ->
            #{
                id => Id,
                start => {socket_handler, start_link, [ListenSocket2]},
                type => worker
            }
        end,
    [ChildSpec(Id, ListenSocket) || Id <- lists:seq(1, 16)].


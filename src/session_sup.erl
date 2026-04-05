-module(session_sup).
-behaviour(supervisor).

%% API
-export([
    start_link/0
]).

%% Callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% api
%%%===================================================================

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% supervisor
%%%===================================================================

init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 1,
        period => 5
    },
    ChildSpecs = [#{
        id => session,
        start => {session, start_link, []},
        restart => temporary
    }],
    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% utils
%%%===================================================================



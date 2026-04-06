-module(session_sup).
-behaviour(supervisor).

%% API
-export([
    start_link/1,
    start_session/3
]).

%% Callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% api
%%%===================================================================

-spec start_link(Controller :: module()) ->
    supervisor:startlink_ret().
start_link(Controller) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Controller).

start_session(SocketPid, Socket, RequestRaw) ->
    case
        supervisor:start_child(session_sup, [
            erlang:self(),
            Socket,
            RequestRaw
        ])
    of
        {ok, _} -> either:right(ok);
        {ok, _, _} -> either:right(ok);
        {error, Reason} -> either:left({error, Reason})
    end.

%%%===================================================================
%%% supervisor
%%%===================================================================

init(Controller) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 1,
        period => 5
    },
    ChildSpecs = [#{
        id => session,
        start => {session, start_link, [Controller]},
        restart => temporary
    }],
    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% utils
%%%===================================================================



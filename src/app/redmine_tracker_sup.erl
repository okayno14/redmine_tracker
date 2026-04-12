%%%-------------------------------------------------------------------
%% @doc redmine_tracker top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(redmine_tracker_sup).

-behaviour(supervisor).

-export([start_link/0]).

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
    %% TODO move to unix_socket_framework_sup when extracting to another library
    ChildSpecs = [
        #{
            id => socket_sup,
            start => {socket_sup, start_link, []},
            type => supervisor
        },
        #{
            id => session_sup,
            start => {session_sup, start_link, [controller_2]},
            type => supervisor
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions


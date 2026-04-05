-module(session).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-define(TIMEOUT, infinity).

%% api
-export([start_link/2]).

%% gen_server
-export([
    init/1,
    handle_continue/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-record(state, {
    handle_pid
}).

-type state() :: #state{}.

%%%===================================================================
%%% api
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
-spec start_link(HandlerPid :: pid(), RequestRaw :: unicode:unicode_binary()) ->
    {ok, pid()} | {error, {already_started, pid()}} | {error, Reason :: any()}.
%%--------------------------------------------------------------------
start_link(HandlerPid, RequestRaw) ->
    {ok, _Pid} = gen_server:start_link(?MODULE, {HandlerPid, RequestRaw}, []).
%%--------------------------------------------------------------------

%%%===================================================================
%%% gen_server
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
-spec init({HandlerPid :: pid(), RequestRaw :: unicode:unicode_binary()}) ->
    {ok, state(), {continue, {handle_request, RequestRaw :: unicode:unicode_binary()}}}.
%%--------------------------------------------------------------------
init({HandlerPid, RequestRaw}) ->
    {ok,
        #state{handle_pid = HandlerPid},
        {continue, {handle_request, RequestRaw}}
    }.
%%--------------------------------------------------------------------

handle_continue({handle_request, RequestRaw}, State) ->
    %% TODO удалить
    ?LOG_DEBUG("RequestRaw:~ts", [RequestRaw]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc
-spec handle_call(Request :: term(), From :: {pid(), _Tag}, State :: state()) ->
    {reply, Result :: ok, State2 :: state()}.
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec handle_cast(Request :: term(), State :: state()) ->
    {noreply, State2 :: state()}.
%%--------------------------------------------------------------------
handle_cast(_Request, State) ->
    {noreply, State}.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec handle_info(Info :: term(), State :: state()) ->
    {noreply, State :: state()}.
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec terminate(Reason :: term(), State :: state()) ->
    ok.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.
%%--------------------------------------------------------------------

%%%===================================================================
%%% gen_server_controller
%%%===================================================================

%%%===================================================================
%%% state api
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
-spec state() ->
    State :: state().
%%--------------------------------------------------------------------
state() ->
    #state{}.
%%--------------------------------------------------------------------



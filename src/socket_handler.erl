-module(socket_handler).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-define(TIMEOUT, infinity).

%% api
-export([start_link/1]).

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
    listen_socket,
    socket
}).

-type state() :: #state{}.

%%%===================================================================
%%% api
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
-spec start_link(ListenSocket :: gen_tcp:socket()) ->
    {ok, pid()}.
%%--------------------------------------------------------------------
start_link(ListenSocket) ->
    {ok, _Pid} = gen_server:start_link(?MODULE, {ListenSocket}, []).
%%--------------------------------------------------------------------

%%%===================================================================
%%% gen_server
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
-spec init({ListenSocket :: gen_tcp:socket()}) ->
    {ok, state(), {continue, {accept, ListenSocket :: gen_tcp:socket()}}}.
%%--------------------------------------------------------------------
init({ListenSocket}) ->
    State = #state{listen_socket = ListenSocket},
    {ok, State, {continue, {accept, ListenSocket}}}.
%%--------------------------------------------------------------------

handle_continue({accept, ListenSocket}, State) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    {noreply, State#state{socket = Socket}}.

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
handle_info({tcp, Socket, RequestRaw}, State = #state{socket = Socket}) ->
    %% TODO если не удалось заспавнить сессию, то надо закрыть коннекцию
    supervisor:start_child(session_sup, [erlang:self(), RequestRaw]),
    {noreply, State};
handle_info(_Info, State) ->
    ?LOG_WARNING("Unknown MSG:~p", [_Info]),
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



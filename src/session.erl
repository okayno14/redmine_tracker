-module(session).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-define(TIMEOUT, infinity).

%% api
-export([start_link/3]).

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
    socket_pid,
    socket
}).

-type state() :: #state{}.

%%%===================================================================
%%% api
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
-spec start_link(
    SocketPid :: pid(), Socket :: pid(), RequestRaw :: unicode:unicode_binary()
) ->
    {ok, pid()} | {error, {already_started, pid()}} | {error, Reason :: any()}.
%%--------------------------------------------------------------------
start_link(SocketPid, Socket, RequestRaw) ->
    {ok, _Pid} = gen_server:start_link(?MODULE, {SocketPid, Socket, RequestRaw}, []).
%%--------------------------------------------------------------------

%%%===================================================================
%%% gen_server
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
-spec init({
    SocketPid :: pid(), Socket :: pid(), RequestRaw :: unicode:unicode_binary()
}) ->
    {ok, state(),
        {continue, {handle_request, RequestRaw :: unicode:unicode_binary()}}}.
%%--------------------------------------------------------------------
init({SocketPid, Socket, RequestRaw}) ->
    {ok,
        #state{socket_pid = SocketPid, socket = Socket},
        {continue, {handle_request, RequestRaw}}
    }.
%%--------------------------------------------------------------------

handle_continue({handle_request, RequestRaw}, State) ->
    handle_request(RequestRaw, State),
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
%%% state_composed
%%%===================================================================

handle_request(RequestRaw, State) ->
    ParseRequest = fun(X) -> either:right(X) end,
    ProcessRequest = fun(X) -> either:right(X) end,
    SendResponse =
        fun(X) ->
            #{state := #state{socket_pid = SocketPid, socket = Socket} = State} = X,
            socket_handler:send_response(SocketPid, Socket, <<"pong"/utf8>>),
            ?LOG_DEBUG("Sent Response to Socket:~p", [Socket]),
            ok
        end,
    compose:compose(
        [
            fun(Either) -> SendResponse(either:extract(Either)) end,
            fun(Either) -> either:flatmap(Either, ProcessRequest) end,
            fun(Either) -> either:flatmap(Either, ParseRequest) end
        ],
        either:right(#{req => RequestRaw, state => State, response => <<"">>})
    ).


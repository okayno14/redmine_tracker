-module(socket_handler).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-define(TIMEOUT, infinity).

%% api
-export([
    start_link/1,
    send_response/3
]).

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

send_response(SocketHandler, Socket, Response) ->
    gen_server:cast(SocketHandler, {send_response, Socket, Response}).

%%%===================================================================
%%% gen_server
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
-spec init({ListenSocket :: gen_tcp:socket()}) ->
    {ok, state(), {continue, accept}}.
%%--------------------------------------------------------------------
init({ListenSocket}) ->
    State = #state{listen_socket = ListenSocket},
    {ok, State, {continue, accept}}.
%%--------------------------------------------------------------------

handle_continue(accept, State) ->
    #state{listen_socket = ListenSocket} = State,
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    ?LOG_DEBUG("Accepted Socket:~p", [Socket]),
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
    {noreply, State2 :: state()} | {noreply, State2 :: state(), {continue, accept}}.
%%--------------------------------------------------------------------
handle_cast({send_response, Socket, Response}, State) ->
    case send_response_(Socket, Response, State) of
        new_socket -> {noreply, State, {continue, accept}};
        ok -> {noreply, State}
    end;
handle_cast(_Request, State) ->
    {noreply, State}.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec handle_info(Info :: term(), State :: state()) ->
    {noreply, State :: state()} | {noreply, State :: state(), {continue, accept}}.
%%--------------------------------------------------------------------
handle_info({tcp, Socket, RequestRaw}, State = #state{}) ->
    compose:if_else(
        fun either:is_right/1,
        fun(X) -> {noreply, either:extract(X)} end,
        fun(X) -> {noreply, either:extract(X), {continue, accept}} end,
        handle_request(Socket, RequestRaw, State)
    );
%% Игнорим, т.к. отправка ответа идемпотентна. С процессом сессии пока ничего не делаем.
handle_info({tcp_closed, _Socket}, State) ->
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
%%% state_composed
%%%===================================================================

handle_request(Socket, RequestRaw, State = #state{socket = Socket}) ->
    Session =
        fun(X) ->
            %% TODO вынести в api session_sup
            case
                supervisor:start_child(session_sup, [
                    erlang:self(),
                    Socket,
                    RequestRaw
                ])
            of
                {ok, _} -> either:right(X);
                {ok, _, _} -> either:right(X);
                {error, Reason} -> either:left({X, {error, Reason}})
            end
        end,
    compose:compose(
        [
            fun(Either) ->
                either:cata(
                    Either,
                    fun({X, Err}) ->
                        ?LOG_WARNING("Failed to spawn session by Error:~p", [Err]),
                        gen_tcp:close(X#state.socket),
                        X
                    end,
                    fun(X) ->
                        #state{socket = Socket} = X,
                        ?LOG_DEBUG("Created Session with Socket:~p", [Socket]),
                        X
                    end
                )
            end,
            fun(Either) -> either:flatmap(Either, Session) end
        ],
        either:right(State)
    );
%% drops msgs for old sockets
handle_request(_Socket, _RequestRaw, State = #state{}) ->
    either:right(State).

%% Если старый сокет - то игнор: коннекция закрыта клиентом или сломалась до завершения сессии - ответ не нужно отдавать.
%% Попробовать отдать ответ, наверху через continue устанавливаем новый сокет
send_response_(Socket, Response, _State = #state{socket = Socket}) ->
    gen_tcp:send(Socket, Response),
    gen_tcp:close(Socket),
    ?LOG_DEBUG("Sent response to socket:~p. Socket closed", [Socket]),
    new_socket;
send_response_(_Socket, _Response, _State) ->
    ok.


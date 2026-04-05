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
    erlang:process_flag(trap_exit, true),
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
terminate(normal, _State) ->
    ok;
terminate(shutdown, _State) ->
    ok;
terminate({shutdown, _}, _State) ->
    ok;
%% If unexpected exception happens - we must send error to socket for connection close
terminate(Reason, State) ->
    ?LOG_ERROR("Reason:~p", [Reason]),
    #state{socket_pid = SocketPid, socket = Socket} = State,
    StackTrace =
        case erlang:process_info(erlang:self(), current_stacktrace) of
            ST when is_list(ST) -> ST;
            _ -> []
        end,
    Msg =
        case
            unicode:characters_to_binary(
                erl_error:format_exception(error, Reason, StackTrace)
            )
        of
            Msg2 when is_binary(Msg2) -> Msg2;
            _ -> <<"Unknown session crash"/utf8>>
        end,
    socket_handler:send_response(
        SocketPid,
        Socket,
        unicode:characters_to_binary(json:encode(response:error_response(Msg)))
    ).
%%--------------------------------------------------------------------

%%%===================================================================
%%% state_composed
%%%===================================================================

handle_request(RequestRaw, State) ->
    ParseRequest =
        fun(X) ->
            #{req := Req} = X,
            try json:decode(Req) of
                #{<<"request">> := _} = Req2 ->
                    ?LOG_DEBUG("Got valid Req:~p", [Req2]),
                    either:right(X#{req => Req2});
                _Json ->
                    either:left(X#{
                        response => response:error_response(
                            <<"Json is not a valid request"/utf8>>
                        )
                    })
            catch
                Err:Reason:StackTrace ->
                    Msg =
                        unicode:characters_to_binary(
                            erl_error:format_exception(Err, Reason, StackTrace)
                        ),
                    either:left(X#{
                        response => response:error_response(Msg)
                    })
            end
        end,
    ProcessRequest = fun(X) -> either:right(X) end,
    SendResponse =
        fun(X) ->
            #{
                state := #state{socket_pid = SocketPid, socket = Socket} = State,
                response := Response
            } = X,
            ?LOG_DEBUG("Response:~p", [Response]),
            %% Even if client sends bad response terminate/1 will handle this
            Msg = unicode:characters_to_binary(json:encode(Response)),
            socket_handler:send_response(SocketPid, Socket, Msg),
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


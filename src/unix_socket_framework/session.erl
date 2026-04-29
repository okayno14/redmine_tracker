-module(session).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-define(TIMEOUT, infinity).

%% api
-export([start_link/4]).

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
    controller,
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
    Controller :: module(),
    SocketPid :: pid(),
    Socket :: pid(),
    RequestRaw :: unicode:unicode_binary()
) ->
    {ok, pid()} | {error, {already_started, pid()}} | {error, Reason :: any()}.
%%--------------------------------------------------------------------
start_link(Controller, SocketPid, Socket, RequestRaw) ->
    {ok, _Pid} = gen_server:start_link(
        ?MODULE, {Controller, SocketPid, Socket, RequestRaw}, []
    ).
%%--------------------------------------------------------------------

%%%===================================================================
%%% gen_server
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
-spec init({
    Controller :: module(),
    SocketPid :: pid(),
    Socket :: pid(),
    RequestRaw :: unicode:unicode_binary()
}) ->
    {ok, state(),
        {continue, {handle_request, RequestRaw :: unicode:unicode_binary()}}}.
%%--------------------------------------------------------------------
init({Controller, SocketPid, Socket, RequestRaw}) ->
    erlang:process_flag(trap_exit, true),
    {ok,
        #state{controller = Controller, socket_pid = SocketPid, socket = Socket},
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
terminate({Reason, StackTrace}, State) ->
    ?LOG_ERROR("Reason:~p", [Reason]),
    #state{socket_pid = SocketPid, socket = Socket} = State,
    Msg =
        unicode:characters_to_binary(
            erl_error:format_exception(error, Reason, StackTrace)
        ),
    %% for eqwalizer
    true = is_binary(Msg),
    socket_handler:send_response(
        SocketPid,
        Socket,
        unicode:characters_to_binary(
            json:encode(response:error_response(session_crashed, Msg))
        )
    ),
    ?LOG_DEBUG("Reason sent to ~p ~p", [SocketPid, Socket]),
    ok;
terminate(Reason, State) ->
    ?LOG_ERROR("Reason:~p", [Reason]),
    #state{socket_pid = SocketPid, socket = Socket} = State,
    {current_stacktrace, StackTrace} =
        erlang:process_info(erlang:self(), current_stacktrace),
    terminate({Reason, StackTrace}, State).
%%--------------------------------------------------------------------

%%%===================================================================
%%% state_composed
%%%===================================================================

handle_request(RequestRaw, State) ->
    ParseRequest =
        fun(X) ->
            #{req := Req} = X,
            try request:'decode!'(Req) of
                {error, not_request} ->
                    either:left(X#{
                        response => response:error_response(
                            invalid_json_request,
                            <<"JSON not a request">>
                        )
                    });
                Req2 ->
                    ?LOG_DEBUG("Got valid Req:~p", [Req2]),
                    either:right(X#{req => Req2})
            catch
                Err:Reason:StackTrace ->
                    Msg =
                        unicode:characters_to_binary(
                            erl_error:format_exception(Err, Reason, StackTrace)
                        ),
                    %% for eqwalizer
                    true = is_binary(Msg),
                    either:left(X#{
                        response => response:error_response(invalid_json_request, Msg)
                    })
            end
        end,
    ProcessRequest =
        fun(X) ->
            #{state := State, req := Req} = X,
            #state{controller = Controller} = State,
            Resp = controller:route(Controller, Req),
            either:right(X#{response => Resp})
        end,
    SendResponse =
        fun(X) ->
            #{
                state := #state{socket_pid = SocketPid, socket = Socket} = State,
                response := Response
            } = X,
            %% Even if client sends bad response terminate/1 will handle this
            Msg = unicode:characters_to_binary(json:encode(Response)),
            socket_handler:send_response(SocketPid, Socket, Msg),
            ?LOG_DEBUG(
                "Sent to Socket:~p Response:\n~ts",
                [Socket, response:format(Response)]
            ),
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


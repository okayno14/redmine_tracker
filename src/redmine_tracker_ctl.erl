-module(redmine_tracker_ctl).

%% API exports
-export([main/1]).

%% escript Entry point
main(Args) ->
    argparse:run(Args, cli(), #{progname => ?MODULE}).

cli() ->
    #{
        arguments => [],
        commands => #{
            "export_to_csv" => #{
                arguments => [],
                handler => fun export_to_csv/1
            },
            "import_from_csv" => #{
                arguments => [
                    #{
                        name => csv,
                        default => <<"stdin">>,
                        type => binary,
                        help => <<"\"stdin\"  or \"Path\" - path to csv-file">>
                    }
                ],
                handler => fun import_from_csv/1
            }
        }
    }.

export_to_csv(#{}) ->
    ProcessResp =
        fun
            (#{type := ok, data := CSV}) ->
                io:format("~ts\n", [CSV]),
                ok;
            (_) ->
                nomatch
        end,
    %% TODO можем откинуться из-за исключения
    Req =
        fun() ->
            either:right(json:encode(#{request => <<"export_to_csv">>}))
        end,
    process_request(Req, ProcessResp).

import_from_csv(#{csv := <<"stdin">>}) ->
    Req =
        fun() ->
            %% TODO можем откинуться из-за исключения
            CSV = unicode:characters_to_binary(read_all_io(standard_io, [])),
            either:right(json:encode(#{request => <<"import_from_csv">>, csv => CSV}))
        end,
    ProcessResp =
        fun
            (#{type := ok, data := Data}) ->
                io:format("~ts\n", [Data]);
            (_) ->
                nomatch
        end,
    process_request(Req, ProcessResp);
import_from_csv(#{csv := Path}) ->
    Req =
        fun() ->
            case file:read_file(Path) of
                {ok, Binary} ->
                    CSV = unicode:characters_to_binary(Binary),
                    either:right(
                        %% TODO можем откинуться из-за исключения
                        %% TODO заменить на либу request
                        json:encode(#{request => <<"import_from_csv">>, csv => CSV})
                    );
                {error, Reason} ->
                    either:left(
                        response:error_response(
                            read_csv_file,
                            unicode:characters_to_binary(file:format_error(Reason))
                        )
                    )
            end
        end,
    ProcessResp =
        fun
            (#{type := ok, data := Data}) ->
                io:format("~ts\n", [Data]);
            (_) ->
                nomatch
        end,
    process_request(Req, ProcessResp).

process_request(Req, ProcessResp) ->
    compose:compose(
        [
            fun(X) ->
                either:map(
                    X,
                    fun(Resp) ->
                        case ProcessResp(Resp) of
                            ok -> ok;
                            nomatch -> format_resp(Resp)
                        end
                    end
                )
            end,
            fun(X) ->
                case {either:is_right(X), either:extract(X)} of
                    {true, Req2} ->
                        either:cata(
                            send_req(Req2),
                            fun(X2) ->
                                format_req_2_error(X2),
                                X2
                            end,
                            fun(X2) -> X2 end
                        );
                    {false, _} ->
                        either:swap(X)
                end
            end
        ],
        Req()
    ).

-spec format_req_2_error(send_req_2_err()) ->
    ok.
format_req_2_error({error, {connect, Reason}}) ->
    io:format("Failed to connect by reason:~ts", [inet:format_error(Reason)]);
format_req_2_error({error, {send, Reason}}) ->
    case Reason of
        closed -> io:format("Failed to send, already closed", []);
        {timeout, _} -> io:format("Failed to send by timeout", []);
        _ -> io:format("Failed to send by reason:~ts", [inet:format_error(Reason)])
    end;
format_req_2_error({error, {recv, Reason}}) ->
    case Reason of
        closed -> io:format("Failed to recv, already closed", []);
        _ -> io:format("Failed to reccv by reason:~ts", [inet:format_error(Reason)])
    end;
format_req_2_error({error, {resp, Resp, bad_response}}) ->
    io:format("Got malformed response from server:\n~ts", [Resp]).

read_all_io(Device, Acc) ->
    io:setopts(standard_io, [binary]),
     case io:get_line(Device, "") of
        eof -> lists:reverse(Acc);
        String -> read_all_io(Device, [String | Acc])
    end.

%% TODO вынести в unix_socket_framework в модуль response (сначала сделать from_json, который отбирает нужные поля и превращает в атомы)
%% потом можно использовать эту функцию как в клиентах, так и в кишках фреймворка для красивой печати логов
format_resp(#{
    type := ok,
    data := Data
}) ->
    io:format("~p", [Data]);
format_resp(#{
    type := error,
    reason := Reason,
    msg := Msg
}) ->
    io:format("reason: ~ts\nmsg: ~ts", [Reason, Msg]).

%% TODO попробовать вытащить из конфиги
send_req(Req) ->
    send_req(<<"/tmp/redmine_tracker.sock">>, Req).

-type send_req_2_err() ::
    {error, {connect, Reason :: inet:posix()}}
    | {error, {send, closed | {timeout, RestData :: binary() | erlang:iovec()} | inet:posix()}}
    | {error, {recv, closed | inet:posix()}}
    | {error, {resp, Resp :: unicode:unicode_binary(), bad_response}}.

-spec send_req(Path :: unicode:unicode_binary(), Req :: unicode:unicode_binary()) ->
    either:either(
        send_req_2_err(),
        response:response()
    ).

send_req(Path, Req) ->
    compose:compose(
        [
            fun(X) ->
                either:flatmap(
                    X,
                    fun(#{resp := Resp, socket := Socket}) ->
                        gen_tcp:close(Socket),
                        either:cata(
                            response:'decode!'(Resp),
                            fun({error, bad_response}) -> {error, {resp, Resp, bad_response}} end,
                            fun(X2) -> X2 end
                        )
                    end
                )
            end,
            fun(X) ->
                either:flatmap(
                    X,
                    fun(Arg = #{socket := Socket}) ->
                        case gen_tcp:recv(Socket, 0) of
                            {ok, Bin} -> either:right(Arg#{resp => Bin});
                            {error, Reason} -> either:left({error, {recv, Reason}})
                        end
                    end
                )
            end,
            fun(X) ->
                either:flatmap(
                    X,
                    fun(#{socket := Socket, req := Req2}) ->
                        case gen_tcp:send(Socket, Req2) of
                            ok -> either:right(#{socket => Socket, req => Req2});
                            {error, Reason} -> either:left({error, {send, {Reason}}})
                        end
                    end
                )
            end,
            fun(X) ->
                either:flatmap(
                    X,
                    fun(#{path := Path2, req := Req2}) ->
                        case gen_tcp:connect({local, Path2}, 0, [{active, false}, binary, {packet, 4}], 1000) of
                            {ok, Socket} -> either:right(#{socket => Socket, req => Req2});
                            {error, Reason} -> either:left({error, {connect, Reason}})
                        end
                    end
                )
            end
        ],
        either:right(#{path => Path, req => Req})
    ).


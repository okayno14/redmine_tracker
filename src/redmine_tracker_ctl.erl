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
                        default => <<"-">>,
                        type => binary,
                        help => <<"\"-\" - stdin or \"Path\" - path to csv-file">>
                    }
                ],
                handler => fun import_from_csv/1
            }
        }
    }.

export_to_csv(#{}) ->
    ProcessResp =
        fun
            (#{<<"type">> := <<"ok">>, <<"data">> := CSV}) ->
                io:format("~ts\n", [CSV]),
                ok;
            (_) ->
                nomatch
        end,
    %% TODO можем откинуться из-за исключения
    SendReq =
        fun() ->
            send_req_2(json:encode(#{request => <<"export_to_csv">>}))
        end,
    process_request(SendReq, ProcessResp).

import_from_csv(#{csv := <<"-">>}) ->
    CSV = unicode:characters_to_binary(read_all_io(standard_io, [])),
    {ok, Resp} = send_req(json:encode(#{request => <<"import_from_csv">>, csv => CSV})),
    %% TODO нужна нормальная печать ошибок
    %% TODO почему-то если 9 строк в csv, то json билдится ок, если 10 - то сервак говорит, что ошибка в парсинге json-а
    #{<<"type">> := <<"ok">>, <<"data">> := Data} = Resp,
    io:format("~ts\n", [Data]);
import_from_csv(Args = #{csv := _File}) ->
    io:format("Args: ~p\n", [Args]).

process_request(SendReq, ProcessResp) ->
    compose:compose(
        [
            %% TODO все функции вызывают процедурно io:format. А что если каждая функция будет возвращать строку, а в конце сделаем печать?
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
                either:cata(
                    X,
                    fun(X2) ->
                        format_req_2_error(X2),
                        X2
                    end,
                    fun(X2) -> X2 end
                )
            end
        ],
        SendReq()
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
    end.


read_all_io(Device, Acc) ->
    io:setopts(standard_io, [binary]),
    case io:get_line(Device, "") of
        eof -> lists:reverse(Acc);
        String -> read_all_io(Device, [String | Acc])
    end.

%% TODO попробовать вытащить из конфиги
send_req(Req) -> send_req(<<"/tmp/redmine_tracker.sock">>, Req).
%% TODO переписать на either с отметками о месте возникновения ошибки,
%% чтобы потом можно было прикрутить для ошибки дефолтный обработчик,
%% а внутри функций обрабатывать resp
%% TODO в обработчике Resp нужно также сделать кастомный код и общий код для дефолтных ошибок - стактрейсов и т.д.
send_req(Path, Req) ->
    maybe
        {ok, Socket} ?= gen_tcp:connect({local, Path}, 0, [{active, false}, binary]),
        ok ?= gen_tcp:send(Socket, Req),
        {ok, Bin} ?= gen_tcp:recv(Socket, 0),
        true ?= is_binary(Bin),
        {ok, json:decode(Bin)}
    end.

%% TODO вынести в unix_socket_framework в модуль response (сначала сделать from_json, который отбирает нужные поля и превращает в атомы)
%% потом можно использовать эту функцию как в клиентах, так и в кишках фреймворка для красивой печати логов
format_resp(#{
    <<"type">> := <<"ok">>,
    <<"data">> := Data
}) ->
    io:format("~p", [Data]);
format_resp(#{
    <<"type">> := <<"error">>,
    <<"reason">> := Reason,
    <<"msg">> := Msg
}) ->
    io:format("reason: ~ts\nmsg: ~ts", [Reason, Msg]).

send_req_2(Req) ->
    send_req_2(<<"/tmp/redmine_tracker.sock">>, Req).

-type send_req_2_err() ::
    {error, {connect, Reason :: inet:posix()}}
    | {error, {send, closed | {timeout, RestData :: binary() | erlang:iovec()} | inet:posix()}}
    | {error, {recv, closed | inet:posix()}}.

-spec send_req_2(Path :: unicode:unicode_binary(), Req :: unicode:unicode_binary()) ->
    either:either(
        send_req_2_err(),
        response:response()
    ).

send_req_2(Path, Req) ->
    compose:compose(
        [
            fun(X) ->
                either:map(
                    X,
                    fun(#{resp := Resp, socket := Socket}) ->
                        %% TODO можем упасть из-за исключения
                        gen_tcp:close(Socket),
                        json:decode(Resp)
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
                        case gen_tcp:connect({local, Path2}, 0, [{active, false}, binary]) of
                            %% TODO ?
                            {ok, Socket} -> either:right(#{socket => Socket, req => Req2});
                            {error, Reason} -> either:left({error, {connect, Reason}})
                        end
                    end
                )
            end
        ],
        either:right(#{path => Path, req => Req})
    ).


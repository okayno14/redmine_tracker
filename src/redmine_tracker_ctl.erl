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
    {ok, Resp} = send_req(json:encode(#{request => <<"export_to_csv">>})),
    #{<<"type">> := <<"ok">>, <<"data">> := CSV} = Resp,
    io:format("~ts\n", [CSV]).

import_from_csv(#{csv := <<"-">>}) ->
    CSV = unicode:characters_to_binary(read_all_io(standard_io, [])),
    {ok, Resp} = send_req(json:encode(#{request => <<"import_from_csv">>, csv => CSV})),
    #{<<"type">> := <<"ok">>, <<"data">> := Data} = Resp,
    io:format("~ts\n", [Data]);
import_from_csv(Args = #{csv := _File}) ->
    io:format("Args: ~p\n", [Args]).

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


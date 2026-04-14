-module(redmine_tracker_ctl).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    argparse:run(Args, cli(), #{progname => ?MODULE}).

cli() ->
    #{
        arguments => [],
        commands => #{
            "export_to_csv" => #{
                arguments => [],
                handler => fun(L) -> io:format("Args: ~p\n", [L]) end
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
                handler => fun(L) -> io:format("Args: ~p\n", [L]) end
            }
        }
    }.

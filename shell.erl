{ok, CSV} = file:read_file("/tmp/track.csv").

L =
either:extract(
    either:flatmap(
        track:from_csv_all(CSV),
        fun(L) ->
            case lists:filter(fun({_Track2, Ret2}) -> Ret2 =/= [] end, [{Track, track:validate(Track)} || Track <- L]) of
                [] -> either:right(L);
                ErrorList -> either:left(ErrorList)
            end
        end
    )
).

Config = application:get_all_env(redmine_tracker).
Push = fun(Track) -> track:push_to_redmine(Track, proplists:get_value(user_id, Config), proplists:get_value(redmine_instance, Config), proplists:get_value(api_key, Config)) end.
[{Push(Track), Track} || Track <- L].

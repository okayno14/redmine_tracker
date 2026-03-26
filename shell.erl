{ok, CSV} = file:read_file("/tmp/track.csv").
L = either:extract(track:from_csv_all(CSV)).

Config = application:get_all_env(redmine_tracker).
Push = fun(Track) -> track:push_to_redmine(Track, proplists:get_value(user_id, Config), proplists:get_value(redmine_instance, Config), proplists:get_value(api_key, Config)) end.
[{Push(Track), Track} || Track <- L].

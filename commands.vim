1,$d | r! _build/default/bin/redmine_tracker_ctl export_to_csv
%!_build/default/bin/redmine_tracker_ctl import_from_csv && _build/default/bin/redmine_tracker_ctl end_track_last

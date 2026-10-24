function ExporToCsv()
    %d
    0r! redmine_tracker_ctl export
    $d
    " move to begin of buffer
    0
endfunction

function ImportFromCsv()
    let ret = systemlist("redmine_tracker_ctl import", getline(1, line('$')))
    %d
    call setline(1, ret)
endfunction

function EndTrackLast()
    ! redmine_tracker_ctl end_track_last
endfunction

function PushToRedmine()
    ! redmine_tracker_ctl push_to_redmine
endfunction

function BeginTrack(project_id, activity_desc, task, ...)
    " '1' - first optional argument
    let desc = get(a:, '1', '""')

    let cmd = "redmine_tracker_ctl begin_track" .. " " .. a:project_id .. " " .. a:activity_desc .. " " .. a:task .. " " .. desc
    echo cmd
    let ret = systemlist(cmd)
    echo join(ret, "\n")
    return ret
endfunction


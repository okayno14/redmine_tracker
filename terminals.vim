if !has('nvim')
    finish
endif

execute "terminal" | RenameBuffer sh | call chansend(b:terminal_job_id, "nix-shell --command \'EDITOR=nvim rebar3 shell\'")
execute "terminal" | RenameBuffer client | call chansend(b:terminal_job_id, "nix-shell --command \'EDITOR=nvim erl\'")
execute "terminal" | RenameBuffer term



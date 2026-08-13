# Overview

redmine_tracker - local assistent for editing redmine time entries.

Features:

* Stores tracks in local Mnesia DB;
* Pushes local tracks to redmine server;
* Exports all local tracks to CSV-format;
* Imports CSV-format to local Mnesia DB.

# Installation

redmine_tracker distributes only with NIX.

## Evaluate derivation

~/.config/nixpkgs/config.nix:

```nix
let
  redmine_tracker = import (builtins.fetchTarball {
        url = "https://github.com/okayno14/redmine_tracker/archive/refs/tags/0.0.1.tar.gz";
  });
in
{
  packageOverrides = pkgs: with pkgs; {
      myPackages = pkgs.buildEnv {
          name = "my-packages";
          paths = [ redmine_tracker ];
      };
  };
}
```

or

```sh
nix-env -i -f https://github.com/okayno14/redmine_tracker/archive/refs/tags/0.0.1.tar.gz
```

or

* clone this repo;
* use command

```sh
nix-env -i -f ./default.nix
```

## Paste default config

~/.config/redmine_tracker/sys.config:

```erlang
[
    {kernel, [
        {logger_level, info},
        {logger, [
            {handler, default, logger_std_h, #{
                filter_default => log,
                formatter => {logger_formatter,
                    #{
                        template => [
                            time, " ",
                            level, " ",
                            pid,
                            " [", mfa, ":", line, "]\: ",
                            msg,
                            "\n"
                        ],
                        single_line => false,
                        time_designator => $\s
                    }
                }
            }}
        ]}
    ]},
    {redmine_tracker, [
      {api_key, <<"SOME_KEY">>},
      {user_id, 1},
      {redmine_instance, <<"SOME_REDMINE_URL">>},
      {activities, #{
       <<"Code">> => 1,
       <<"Code Review">> => 2,
       <<"Test">> => 3,
       <<"Documentation">> => 4
      }}
    ]}
].
```

## Launch release

```sh
redmine_tracker_startup foreground
```

or

```sh
redmine_tracker_startup console
```

# Usage

Base Workflow:

* `redmine_tracker_ctl begin_track ...` - redmine_tracker remembers starting point for task. Creates a new **track**;
* ...
* `redmine_tracker_ctl end_track_last` - redmine_tracker commits final endpoint for task. Finalize a created **track**;
* `redmine_tracker_ctl push_to_redmine` - redmine_tracker pushes all **tracks** to redmine

```sh
redmine_tracker_ctl --help
```


let
    pkgs_otp27 = import (builtins.fetchTarball {
          url = "https://github.com/NixOS/nixpkgs/archive/e6f23dc08d3624daab7094b701aa3954923c6bbb.tar.gz";
    }) {};
    name = "redmine_tracker";
    pname = name;
    version = "0.0.1";
    src = ./.;
in
  with pkgs_otp27;
    (
      beam27Packages.rebar3Relx {
        inherit pname version src;
        profile = "prod";
        releaseType = "release";
        checkouts =
          (beam27Packages.fetchRebar3Deps {
            inherit name version src;
            sha256 = "sha256-qz8wN2e7eOHpZxK9V0wXgzMBBcbS1w3TsbLQxr9MKn0=";
          }).overrideAttrs (old: {
            nativeBuildInputs = (old.nativeBuildInputs or []) ++ [ git ];
            # .git/hooks/ contains scripts with nix paths
            preInstall = ''
              rm -rf ./_build/default/lib/*/.git/
            '';
          });
      }
    )

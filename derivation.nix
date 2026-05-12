let
    pkgs_otp27 = import (builtins.fetchTarball {
          url = "https://github.com/NixOS/nixpkgs/archive/e6f23dc08d3624daab7094b701aa3954923c6bbb.tar.gz";
    }) {};
in
  with pkgs_otp27;
    (
      beam27Packages.rebar3Relx {
        name = "redmine_tracker";
        pname = "redmine_tracker";
        version = "0.0.1";
        profile = "prod";
        releaseType = "release";
        src = ./.;
        checkouts =
          callPackage ./fetch-rebar3-deps.nix {} {
            # TODO убрать копипасту
            name = "redmine_tracker";
            version = "0.0.1";
            src = ./.;
            sha256 = "sha256-plUDn1sKZKlgcw0q5kpkhtxs2ifN50lyBpjgdpi3lZY=";
            nativeBuildInputs = [ git ];
          };
      }
    ).overrideAttrs (old: {
      postInstall = ''
        echo "Deps fetched to $checkouts"
      '';
    })

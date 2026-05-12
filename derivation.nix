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
            sha256 = "sha256-plUDn1sKZKlgcw0q5kpkhtxs2ifN50lyBpjgdpi3lZY=";
          }).overrideAttrs (old: {
            nativeBuildInputs = [ git ];
          });
      }
    ).overrideAttrs (old: {
      postInstall = ''
        echo "Deps fetched to $checkouts"
      '';
    })

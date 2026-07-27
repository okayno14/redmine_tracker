let
  pkgs_otp27 = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/e6f23dc08d3624daab7094b701aa3954923c6bbb.tar.gz";
  }) { };
  deps = pkgs_otp27.callPackage ./deps2.nix { };
  name = "redmine_tracker";
  pname = name;
  version = "0.0.2";
  src = ./.;
in
with pkgs_otp27;
(
  beam27Packages.rebar3Relx {
    inherit pname version src;
    profile = "prod";
    releaseType = "release";
    checkouts = (
      deps {
        src = lib.fileset.toSource {
          root = ./.;
          fileset = lib.fileset.unions [
            ./rebar.lock
            ./rebar.config
          ];
        };
        inherit name version;
        sha256 = "sha256-WRVHCVcVdA/qpTNHbSehNsC7UKGBI/DYtSEOWa3htkQ=";
      }
    );
  }
)

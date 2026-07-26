let
    pkgs_otp27 = import (builtins.fetchTarball {
          url = "https://github.com/NixOS/nixpkgs/archive/e6f23dc08d3624daab7094b701aa3954923c6bbb.tar.gz";
    }) {};
    name = "redmine_tracker";
    pname = name;
    version = "0.0.2";
    src = ./.;

    # Refactor: looks bad
    # deps = import ./deps.nix {
    #   lib = pkgs_otp27.lib;
    #   stdenvNoCC = pkgs_otp27.stdenvNoCC;
    #   rebar3 = pkgs_otp27.rebar3;
    #   git = pkgs_otp27.git;
    #   coreutils = pkgs_otp27.coreutils;
    # };
    deps = import ./deps2.nix {
      stdenv = pkgs_otp27.stdenv;
      rebar3 = pkgs_otp27.rebar3;
      git = pkgs_otp27.git;
      cacert = pkgs_otp27.cacert;
    };
    # rebar3 = pkgs_otp27.beam27Packages.rebar3.overrideAttrs(
    #   old: {nativeBuildInputs = [pkgs_otp27.git];}
    # );
in
  with pkgs_otp27;
    (

      # (beam27Packages.fetchRebar3Deps {
      #   inherit name version src;
      #   sha256 = "sha256-jv0kVdpsaISvBF60PYZXw0QGyXHcrJyqLB83Ptv8kMw=";
      # }).overrideAttrs (old: {
      #   nativeBuildInputs = [ git ];
      # })

      # .overrideAttrs (old: {
          # preBuild = (old.preBuild or "") + ''
          #   export PATH="${git}/bin:$PATH"
          # '';
          # postInstall = ''
          #   echo "Deps fetched to $out"
          # '';
      # })
      (beam27Packages.rebar3Relx {
        inherit pname version src;
        profile = "prod";
        releaseType = "release";
        checkouts =
          (deps {
              src = lib.fileset.toSource {
                root = ./.;
                fileset = lib.fileset.unions [
                  ./rebar.lock
                  ./rebar.config
                ];
              };
              inherit name version;
              sha256 = "sha256-WRVHCVcVdA/qpTNHbSehNsC7UKGBI/DYtSEOWa3htkQ=";
          });
      }).overrideAttrs {
          # Added, cause rebar3 can't make a dir inside fetched directory
          # TODO no ${profile} reuse
          preBuild = ''
            mkdir -p _build/prod/rel/redmine_tracker/bin
            ls -alF .
            ls -alF _build/default/bin
            ls -alF _build/prod/rel/redmine_tracker/
          '';
        }
    )

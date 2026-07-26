let
    pkgs_otp27 = import (builtins.fetchTarball {
          url = "https://github.com/NixOS/nixpkgs/archive/e6f23dc08d3624daab7094b701aa3954923c6bbb.tar.gz";
    }) {};
    name = "redmine_tracker";
    pname = name;
    version = "0.0.1";
    src = ./.;

    # Refactor: looks bad
    deps = import ./deps.nix {
      lib = pkgs_otp27.lib;
      stdenv = pkgs_otp27.stdenv;
      rebar3 = pkgs_otp27.rebar3;
      git = pkgs_otp27.git;
    };
    # rebar3 = pkgs_otp27.beam27Packages.rebar3.overrideAttrs(
    #   old: {nativeBuildInputs = [pkgs_otp27.git];}
    # );
in
  with pkgs_otp27;
    (

      # (beam27Packages.fetchRebar3Deps {
      #   inherit name version src;
      #   sha256 = "sha256-plUDn1sKZKlgcw0q5kpkhtxs2ifN50lyBpjgdpi3lZY=";
      # }).overrideAttrs (old: {
      #   nativeBuildInputs = [ git ];
      # })


      beam27Packages.rebar3Relx {
        inherit pname version src;
        profile = "prod";
        releaseType = "release";
        checkouts = deps {
            src = lib.fileset.toSource {
              root = ./.;
              fileset = lib.fileset.unions [
                ./rebar.lock
                ./rebar.config
              ];
            };
            inherit name version;
            sha256 = "sha256-jv0kVdpsaISvBF60PYZXw0QGyXHcrJyqLB83Ptv8kMw=";
        };
        # checkouts =
        #   beam27Packages.fetchRebar3Deps {
        #     inherit name version src;
        #     # sha256 = "sha256-plUDn1sKZKlgcw0q5kpkhtxs2ifN50lyBpjgdpi3lZY=";
        #     # sha256 = "sha256-rF2dFDk5xY5+sGetaF0PK5/aK1cyZ9JBDGimxOQz3JE=";
        #     sha256 = "sha256-jv0kVdpsaISvBF60PYZXw0QGyXHcrJyqLB83Ptv8kMw=";
        #   };
          # ();
      }
    ).overrideAttrs (old: {
      postInstall = ''
        echo "Deps fetched to $checkouts"
      '';
    })

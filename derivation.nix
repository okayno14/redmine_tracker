let
    pkgs_otp27 = import (builtins.fetchTarball {
          url = "https://github.com/NixOS/nixpkgs/archive/e6f23dc08d3624daab7094b701aa3954923c6bbb.tar.gz";
    }) {};
in
  with pkgs_otp27;
    let
      a = "faa";
      # TODO use common map and inherit keys
      # deps = fetchRebar3Deps {
      #   name = "redmine_tracker";
      #   version = "0.0.1";
      #   src = ./.;
      #   sha256 = "0000000000000000000000000000000000000000000000000000";
      # };
    in
      callPackage ./fetch-rebar3-deps.nix {} {
        name = "redmine_tracker";
        version = "0.0.1";
        src = ./.;
        sha256 = "sha256-plUDn1sKZKlgcw0q5kpkhtxs2ifN50lyBpjgdpi3lZY=";
        nativeBuildInputs = [ git ];
      }
      # beam27Packages.rebar3Relx {
      #   pname = "redmine_tracker";
      #   version = "0.0.1";
      #   profile = "prod";
      #   releaseType = "release";
      #   src = ./.;
      #   # nativeBuildInputs = [
      #   #   beam27Packages.erlang
      #   #   beam27Packages.rebar3
      #   # ];
      #   # buildInputs = [
      #   #   beam27Packages.erlang
      #   # ];
      #   # buildPhase = ''
      #   #   # echo $USER
      #   #   # echo $PWD
      #   #   # export HOME=$TMPDIR
      #   #   # export DEBUG=1
      #   #   # rebar3 get-deps
      #   #   # rebar3 compile
      #   #   rebar3 release
      #   # '';
      #   # deps = beam27Packages.fetchRebar3Deps {
      #   #   rebarLock = ./rebar.lock;
      #   # };
      # }


let
    pkgs_otp27 = import (builtins.fetchTarball {
          url = "https://github.com/NixOS/nixpkgs/archive/e6f23dc08d3624daab7094b701aa3954923c6bbb.tar.gz";
    }) {};
in

pkgs_otp27.mkShell {
  packages = with pkgs_otp27; [
    gcc
    beam27Packages.erlang
    beam27Packages.rebar3
    erlang-language-platform
    beam27Packages.erlfmt
  ];
}



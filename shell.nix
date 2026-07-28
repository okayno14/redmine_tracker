let
  inputs = import ./inputs.nix;
  pkgs_otp27 = inputs.pkgs_otp27;
in
pkgs_otp27.mkShell {
  packages = with pkgs_otp27; [
    # TODO remove?
    gcc
    beam27Packages.erlang
    beam27Packages.rebar3
    erlang-language-platform
    beam27Packages.erlfmt
  ];
}

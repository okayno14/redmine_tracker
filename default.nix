let
  inputs = import ./inputs.nix;
  pkgs_otp27 = inputs.pkgs_otp27;
  name = inputs.name;
  pname = inputs.pname;
  version = inputs.version;
  src = ./.;
in
with pkgs_otp27;
(beam27Packages.rebar3Relx {
  inherit pname version src;
  profile = "prod";
  releaseType = "release";
  checkouts =
    (beam27Packages.fetchRebar3Deps {
      inherit name version src;
      sha256 = "sha256-qz8wN2e7eOHpZxK9V0wXgzMBBcbS1w3TsbLQxr9MKn0=";
    }).overrideAttrs
      (old: {
        nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ git ];
        # .git/hooks/ contains scripts with nix paths
        preInstall = ''
          rm -rf ./_build/default/lib/*/.git/
        '';
      });
})

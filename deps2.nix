{
  stdenv,
  rebar3,
  git,
  cacert
}:
# { pkgs ? import <nixpkgs> {} }:

{
  src,
  name,
  version,
  sha256
}:
stdenv.mkDerivation {
  pname = "my-erlang-deps-${name}";
  inherit version;
  inherit src;
  # src = ./.; # Needs to contain rebar.config

  # cacert is mandatory for SSL verification during git clone
  nativeBuildInputs = [ rebar3 git cacert ];

  buildPhase = ''
    ls -alF .
    echo "test"
    ls -alF "$src"
    cp "$src/rebar.config" .
    cp "$src/rebar.lock" .
    rm -rf .git
    rebar3 get-deps
  '';

  installPhase = ''
    ls -alF .
    ls -alF _build/default/lib
    mkdir -p $out

    rm -rf _build/default/lib/erl_utils/.git
    cp -r _build/default/lib/erl_utils "$out/"
    ls -alF "$out/erl_utils"

    rm -rf _build/default/lib/meck/.git*
    rm _build/default/lib/meck/.scripts/cut.sh
    cp -r _build/default/lib/meck "$out/"
    ls -alF "$out/meck"

    # rm -rf _build/default/lib/*/.git
    # cp -r _build/default/lib/* "$out/"

    ls -alF "$out/"
  '';

  # Fixed-Output Derivation settings
  outputHashAlgo = "sha256";
  outputHashMode = "recursive";
  outputHash = sha256;
  dontUnpack = true;
  # dontPatch = true;
}

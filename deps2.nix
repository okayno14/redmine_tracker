{
  stdenv,
  rebar3,
  git,
  cacert,
}:

{
  src,
  name,
  version,
  sha256,
}:
stdenv.mkDerivation {
  pname = "my-erlang-deps-${name}";
  inherit version src;

  # cacert is mandatory for SSL verification during git clone
  nativeBuildInputs = [
    rebar3
    git
    cacert
  ];

  buildPhase = ''
    cp "$src/rebar.config" .
    cp "$src/rebar.lock" .
    rm -rf .git
    rebar3 get-deps
  '';

  installPhase = ''
    mkdir -p "$out/_checkouts"

    rm -rf _build/default/lib/erl_utils/.git
    cp -r _build/default/lib/erl_utils "$out/_checkouts"

    rm -rf _build/default/lib/meck/.git*
    rm _build/default/lib/meck/.scripts/cut.sh
    cp -r _build/default/lib/meck "$out/_checkouts"

    # rm -rf _build/default/lib/*/.git
    # cp -r _build/default/lib/* "$out/"
  '';

  # Fixed-Output Derivation settings
  outputHashAlgo = "sha256";
  outputHashMode = "recursive";
  outputHash = sha256;
  dontUnpack = true;
  # dontFixup = true;
}

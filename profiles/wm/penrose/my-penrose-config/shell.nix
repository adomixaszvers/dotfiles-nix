{ pkgs ? import <nixpkgs> { } }:
with pkgs;
mkShell {
  buildInputs =
    [ cargo cargo-edit rustc rustPackages.clippy rustPackages.rustfmt rls ];
  shellHook = ''
    export RUST_SRC_PATH="${rustPlatform.rustcSrc}"
    export RACER_CMD="${rustracer}/bin/racer"
  '';
}

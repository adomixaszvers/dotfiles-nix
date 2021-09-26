{ pkgs ?
  (builtins.getFlake "nixpkgs").legacyPackages."${builtins.currentSystem}" }:
with pkgs;
mkShell {
  buildInputs = [
    cargo
    cargo-edit
    rustc
    rustPackages.clippy
    rustPackages.rustfmt
    rls
    python3
    pkg-config
    cairo
    glib
    pango
    xorg.libxcb
  ];
  shellHook = ''
    export RUST_SRC_PATH="${rustPlatform.rustcSrc}"
    export RACER_CMD="${rustracer}/bin/racer"
  '';
}

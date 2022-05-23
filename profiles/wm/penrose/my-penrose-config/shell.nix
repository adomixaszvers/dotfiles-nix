{ pkgs ?
  (builtins.getFlake "nixpkgs").legacyPackages."${builtins.currentSystem}" }:
with pkgs;
mkShell {
  name = "my-penrose-shell";
  buildInputs = [
    cargo
    cargo-edit
    rustc
    rustPackages.clippy
    rustPackages.rustfmt
    rust-analyzer
    rustPlatform.rustcSrc
    python3
    pkg-config
    cairo
    glib
    pango
    xorg.libxcb
  ];
}

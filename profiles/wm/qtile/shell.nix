{ pkgs ? import <nixpkgs> { } }:
with pkgs;
mkShell {
  name = "qtile-shell";
  buildInputs = with pkgs.nixos-unstable.python3Packages; [
    qtile
    black
    python-language-server
    xlib
  ];
}

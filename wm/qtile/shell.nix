{ pkgs ? import <nixpkgs> { } }:
with pkgs;
mkShell {
  name = "qtile-shell";
  buildInputs = with pkgs.nixos-unstable.python3Packages; [
    python-language-server
    black
  ];
}

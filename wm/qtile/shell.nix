{ pkgs ? import <nixpkgs> { } }:
with pkgs;
mkShell {
  name = "qtile-shell";
  buildInputs = with pkgs.channels.nixos-unstable.python3Packages; [
    python-language-server
    black
  ];
}

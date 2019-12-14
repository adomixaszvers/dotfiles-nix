{ pkgs ? import <nixpkgs> { } }:

with pkgs;

mkShell {
  name = "awesomewm-shell";
  buildInputs = [ luaPackages.luacheck ];
}

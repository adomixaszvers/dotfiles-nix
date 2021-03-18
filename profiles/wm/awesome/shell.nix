{ system ? builtins.currentSystem
, pkgs ? (builtins.getFlake "nixpkgs").legacyPackages."${system}"
, mine ? builtins.getFlake "mine" }:

with pkgs;

mkShell {
  name = "awesomewm-shell";
  buildInputs = [ luaPackages.luacheck mine.packages."${system}".lua-fmt ];
}

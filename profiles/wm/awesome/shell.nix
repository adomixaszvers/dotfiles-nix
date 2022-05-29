{ system ? builtins.currentSystem
, pkgs ? (builtins.getFlake "nixpkgs").legacyPackages."${system}" }:

with pkgs;

mkShell {
  name = "awesomewm-shell";
  buildInputs = [ luaPackages.luacheck stylua ];
}

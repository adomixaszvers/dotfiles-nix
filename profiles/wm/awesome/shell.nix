{
  pkgs ? import <nixpkgs> { },
}:

with pkgs;

mkShellNoCC {
  name = "awesomewm-shell";
  buildInputs = [
    luaPackages.luacheck
    stylua
  ];
}

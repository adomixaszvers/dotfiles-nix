{
  pkgs ? import <nixpkgs> { },
}:

with pkgs;

mkShellNoCC {
  name = "awesomewm-shell";
  packages = [
    luaPackages.luacheck
    stylua
  ];
}

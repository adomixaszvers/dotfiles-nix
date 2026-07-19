{
  pkgs ? import <nixpkgs> { },
}:

with pkgs;

mkShellNoCC {
  name = "awesomewm-shell";
  packages = [
    # keep-sorted start
    luaPackages.luacheck
    stylua
    # keep-sorted end
  ];
}

{
  pkgs ? import <nixpkgs> { },
}:

pkgs.mkShellNoCC {
  name = "awesomewm-shell";
  packages = [
    # keep-sorted start
    pkgs.luaPackages.luacheck
    pkgs.stylua
    # keep-sorted end
  ];
}

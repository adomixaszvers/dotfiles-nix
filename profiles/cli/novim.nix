{ pkgs, ... }:
let
  fakeEditor =
    name:
    pkgs.writeShellScriptBin name ''
      echo "no ${name} for you. use kakoune for now"
      exit 1
    '';
in
{
  home.packages = [
    (fakeEditor "nvim")
    (fakeEditor "vim")
  ];
}

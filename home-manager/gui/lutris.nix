{ pkgs, ... }:
let
  inherit (pkgs) proton-ge-bin;
in
{
  programs.lutris = {
    enable = true;
    defaultWinePackage = proton-ge-bin;
    protonPackages = [ proton-ge-bin ];
    winePackages = [ pkgs.wineWow64Packages.full ];
  };
}

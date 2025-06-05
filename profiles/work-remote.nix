{ pkgs, ... }:
{
  imports = [
    ./work-common.nix
    ./wm/xsession-common.nix
    ./wm/bspwm
  ];
  stylix.base16Scheme = "${pkgs.base16-schemes}/share/themes/dracula.yaml";
  xsession.numlock.enable = true;
  services = {
    picom.enable = false;
    unclutter.enable = false;
  };
}

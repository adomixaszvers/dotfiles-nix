{ ... }:
{
  imports = [
    ./work-common.nix
    ./wm/xsession-common.nix
    ./wm/bspwm
  ];
  xsession.numlock.enable = true;
  services = {
    picom.enable = false;
    unclutter.enable = false;
  };
}

{ ... }:
{
  imports = [
    ./work-common.nix
    ./wm/xsession-common.nix
    ./wm/xmonad
  ];
  xsession.numlock.enable = true;
  services = {
    picom.enable = false;
    unclutter.enable = false;
  };
}

{ config, lib, pkgs, ... }:

{
  imports = [ ./xmonad ./polybar.nix ./dunst.nix ./compton.nix ];
  services.network-manager-applet.enable = config.xsession.enable;
  services.udiskie.enable = true;
  xsession.enable = lib.mkDefault true;
  xsession.initExtra = ''
    xset s off -dpms
  '';
  xsession.pointerCursor = {
    name = "capitaine-cursors";
    package = pkgs.capitaine-cursors;
  };
  xsession.scriptPath = ".xsession-hm";
}

{ config, lib, pkgs, ... }:

{
  imports = [ ./xmonad ./polybar.nix ./dunst.nix ./compton.nix ];
  xsession.enable = lib.mkDefault true;
  xsession.initExtra = ''
    xset s off -dpms
  '';
  xsession.pointerCursor = {
    name = "capitaine-cursors";
    package = pkgs.capitaine-cursors;
  };
}

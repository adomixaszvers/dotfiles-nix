{ pkgs, lib, ... }:
let inherit (pkgs) xdg-desktop-portal-gtk;
in {
  xdg.portal = {
    enable = true;
    configPackages = lib.mkDefault [ xdg-desktop-portal-gtk ];
    extraPortals = [ xdg-desktop-portal-gtk ];
    wlr.enable = true;
  };
}

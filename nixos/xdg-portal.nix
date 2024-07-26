{ pkgs, ... }:
let
  inherit (pkgs) xdg-desktop-portal-gtk;
in
{
  xdg.portal = {
    enable = true;
    config.common.default = "gtk";
    extraPortals = [ xdg-desktop-portal-gtk ];
    wlr.enable = true;
  };
}

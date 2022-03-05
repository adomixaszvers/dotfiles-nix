{ lib, ... }:

{
  imports =
    [ ./work-common.nix ./common.nix ./wm/xsession-common.nix ./wm/xmonad ];
  colors = import ./gui/colors/dracula.nix;
  programs.rofi.theme = lib.mkForce "Arc";
}

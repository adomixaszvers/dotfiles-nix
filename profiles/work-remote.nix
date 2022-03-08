{ lib, ... }:

{
  imports = [ ./work-common.nix ];
  colors = import ./gui/colors/dracula.nix;
  programs.rofi.theme = lib.mkForce "Arc";
  programs.rofi.extraConfig.dpi = 120;
  xresources.properties."Xft.dpi" = 120;
  services.polybar.config = {
    "bar/top-extra".dpi = 120;
    "bar/top".dpi = 120;
  };
}

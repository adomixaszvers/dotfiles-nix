{ lib, ... }:
let dpi = 120;
in {
  imports = [ ./work-common.nix ];
  colors = import ./gui/colors/dracula.nix;
  programs.rofi.theme = lib.mkForce "Arc";
  programs.rofi.extraConfig = { inherit dpi; };
  xresources.properties."Xft.dpi" = dpi;
  services.polybar.config = {
    "bar/top-extra" = { inherit dpi; };
    "bar/top" = { inherit dpi; };
  };
}

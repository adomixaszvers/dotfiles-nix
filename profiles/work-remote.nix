{ lib, ... }: {
  imports = [ ./work-common.nix ];
  colors = import ./gui/colors/dracula.nix;
  programs.rofi.theme = lib.mkForce "Arc";
}

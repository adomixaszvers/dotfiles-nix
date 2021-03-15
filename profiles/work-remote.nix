{ lib, ... }:

{
  imports = [ ./work.nix ];
  programs.rofi.theme = lib.mkForce "Arc";
  services.gpg-agent.enable = lib.mkForce false;
  services.picom.enable = false;
  services.screen-locker.enable = lib.mkForce false;
  services.sxhkd.keybindings = { "super + ctrl + r" = "bspc wm -r"; };
  xsession.windowManager.bspwm.monitors = {
    "rdp0" = [ "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" ];
    "MONITOR" = [ "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" ];
  };
}

{ pkgs, lib, config, ... }:
let extraPackages = import ./extraPackages.nix;
in {
  home.packages = with pkgs; [
    gnome3.zenity
    mine.maimpick
    mine.rofi-powermenu
  ];
  services.polybar.config = {
    "bar/top".modules-left = "xmonad";
    "bar/top-extra".modules-left = "xmonad";
    "module/xmonad" = {
      type = "custom/script";
      exec = "${pkgs.xmonad-log}/bin/xmonad-log";
      tail = true;
    };
  };
  xsession.windowManager.xmonad = {
    inherit extraPackages;
    enable = true;
    config = ./my-xmonad/xmonad.hs;
  };
}

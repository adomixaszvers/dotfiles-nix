{ pkgs, lib, config, ... }:
let
  extraPackages = hs: with hs; [ xmonad xmonad-contrib dbus utf8-string ];
in {
  home.packages = with pkgs; [
    (ghc.withPackages extraPackages)
    (with import <nixos-unstable> { }; haskellPackages.brittany)
    gnome3.zenity
    haskellPackages.hlint
    mine.maimpick
    mine.rofi-powermenu
  ];
  services.polybar.config = {
    "bar/top".modules-left = "xmonad";
    "bar/top-extra".modules-left = "xmonad";
  };
  xsession.windowManager.xmonad = {
    inherit extraPackages;
    enable = true;
    config = ./dotfiles/my-xmonad/xmonad.hs;
  };
}

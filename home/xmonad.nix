{ pkgs, lib, config, ... }:
let
  extraPackages = hs:
    with hs; [
      dbus
      ghcid
      hlint
      xmonad
      xmonad-contrib
      xmonad-extras
    ];
in {
  home.packages = with pkgs; [
    (ghc.withHoogle extraPackages)
    (with import <nixos-unstable> { }; haskellPackages.brittany)
    cabal-install
    gnome3.zenity
    mine.rofi-powermenu
  ];
  xsession.windowManager.xmonad = {
    inherit extraPackages;
    enable = true;
    enableContribAndExtras = true;
    config = ./dotfiles/xmonad.hs;
  };
}

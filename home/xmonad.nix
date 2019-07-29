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
    (with import <nixos-unstable> { }; haskellPackages.brittany)
    cabal-install
    mine.rofi-powermenu
    (ghc.withHoogle extraPackages)
    xorg.xmessage
  ];
  xsession.windowManager.xmonad = {
    inherit extraPackages;
    enable = true;
    enableContribAndExtras = true;
    config = ./dotfiles/xmonad.hs;
  };
}

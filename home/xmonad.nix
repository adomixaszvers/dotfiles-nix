{ pkgs, lib, config, ... }: {
  home.packages = with pkgs; [
    (with import <nixos-unstable> { }; haskellPackages.brittany)
    cabal-install
    mine.rofi-powermenu
    (ghc.withPackages
    (hs: with hs; [ hlint ghcid xmonad xmonad-contrib xmonad-extras ]))
    xorg.xmessage
  ];
  xsession.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    config = ./dotfiles/xmonad.hs;
  };
}

{ pkgs, lib, config, ... }: {
  home.file.".xmonad/build" = {
    executable = true;
    source = ./dotfiles/my-xmonad/build;
  };
  home.packages = with pkgs; [
    (with import <nixos-unstable> { }; haskellPackages.brittany)
    (with import <all-hies> {  }; versions.ghc865)
    gnome3.zenity
    haskellPackages.hlint
    mine.maimpick
    mine.rofi-powermenu
    stack
  ];
  xsession.windowManager.xmonad = { enable = true; config = ./dotfiles/my-xmonad/xmonad.hs; };
}

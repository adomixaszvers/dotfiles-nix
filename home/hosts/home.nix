{ pkgs, ... }:
{
  imports = [ ../emacs.nix ];
  home.packages = with pkgs;
  [
    borgbackup
    discord
    exercism
    firefox
    guile
    gtypist
    klavaro
    mine.steam
    mine.vimgolf
    qbittorrent
    torbrowser
    vim
    vlc
  ];
}

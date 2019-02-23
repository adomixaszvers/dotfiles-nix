{ pkgs, ... }:
{
  imports = [ ../emacs.nix ];
  home.packages = with pkgs;
  [
    borgbackup
    discord
    exercism
    firefox
    gtypist
    mine.steam
    mine.vimgolf
    qbittorrent
    torbrowser
    vim
    vlc
  ];
}

{ pkgs, ... }:
{
  home.packages = with pkgs;
  [
    borgbackup
    discord
    exercism
    firefox
    gtypist
    guile
    jetbrains.idea-ultimate
    klavaro
    mine.steam
    mine.vimgolf
    openjdk11
    qbittorrent
    torbrowser
    vim
    vlc
  ];
  services.kdeconnect = {
    enable = true;
    indicator = true;
  };
}

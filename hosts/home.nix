{ pkgs, config, ... }:
let unstable = import <nixos-unstable> { };
in {
  home.packages = with pkgs; [
    borgbackup
    calibre
    discord
    exercism
    firefox
    gtypist
    guile
    unstable.jetbrains.idea-ultimate
    klavaro
    (mine.steam.override { config.steam.primus = true; })
    mine.vimgolf
    minecraft
    openjdk11
    qbittorrent
    torbrowser
    vim
  ];
}

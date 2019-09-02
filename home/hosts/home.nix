{ pkgs, config, ... }: {
  home.packages = with pkgs; [
    borgbackup
    calibre
    discord
    exercism
    firefox
    gtypist
    guile
    jetbrains.idea-ultimate
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

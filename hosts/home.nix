{ pkgs, config, ... }: {
  home.packages = with pkgs; [
    borgbackup
    calibre
    discord
    exercism
    firefox
    gtypist
    guile
    klavaro
    minecraft
    (mine.steam.override { config.steam.primus = true; })
    mine.vimgolf
    qbittorrent
    torbrowser
    jetbrains.idea-ultimate
    vim
  ];
}

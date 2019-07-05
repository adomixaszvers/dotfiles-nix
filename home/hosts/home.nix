{ pkgs, ... }: {
  home.packages = let mine = import <mine> { inherit pkgs; };
  in with pkgs; [
    borgbackup
    calibre
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

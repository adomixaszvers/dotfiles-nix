{ pkgs, ... }: {
  home.packages = with pkgs; [
    borgbackup
    exercism
    firefox
    gnome3.gnome-boxes
    gtypist
    guile
    klavaro
    mine.vimgolf
    openjdk11
    qbittorrent
    torbrowser
    vim
    vlc
  ];
}

{ pkgs, ... }:
{
  home.packages = with pkgs;
  [
    discord
    exercism
    mine.ghc
    stack
    # torbrowser
    vim
    mine.vimgolf
    vlc
    qbittorrent
    typora
  ];
}

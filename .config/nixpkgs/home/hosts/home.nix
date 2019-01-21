{ pkgs, ... }:
{
  home.packages = with pkgs;
  [
    # sageWithDoc
    borgbackup
    discord
    elmPackages.elm
    exercism
    firefox
    jdk
    jetbrains.idea-ultimate
    mine.ghc
    mine.steam
    mine.vimgolf
    qbittorrent
    sbt
    stack
    torbrowser
    typora
    vim
    vlc
  ];
  lib = {
    lsc = {
      enable = true;
    };
  };
}

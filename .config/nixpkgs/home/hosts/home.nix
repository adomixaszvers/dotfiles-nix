{ pkgs, ... }:
{
  home.packages = with pkgs;
  [
    # sageWithDoc
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
  lib.enableHie = true;
  lib.enablePyls = true;
}

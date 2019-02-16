{ pkgs, ... }:
{
  imports = [ ../emacs.nix ];
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
    (with import <hie-nix> {}; hies)
    mine.vimgolf
    qbittorrent
    sbt
    stack
    torbrowser
    # typora
    vim
    vlc
  ];
}

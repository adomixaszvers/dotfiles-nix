{ pkgs, ... }:
with pkgs; 
let
  ghcCustom = (ghc.withPackages (haskellPackages: with haskellPackages; [
        # ghc-mod
        # leksah
        hasktags
        hdevtools
        hindent
        hlint
        hoogle
        hspec
        pointfree pointful
        stylish-haskell
      ]));
      consul = (callPackage ./consul {});
      vimgolf = (callPackage ./vimgolf {});
      bumblebee-status = (callPackage ./bumblebee-status {});
    in
    [
    # skype
    # typora
    arandr
    atom
    bumblebee-status
    calibre
    consul
    discord
    exercism
    file
    ghcCustom
    git
    gnome3.adwaita-icon-theme
    gnome3.gnome-screenshot
    google-chrome
    htop
    jetbrains.idea-ultimate
    keepass
    klavaro
    lxappearance
    meld
    ncdu
    nodejs
    notify-osd
    okular
    qbittorrent
    ranger
    rxvt_unicode-with-plugins
    smartgithg
    stack
    thefuck
    torbrowser
    tree
    vcsh
    viber
    vim
    vimgolf
    vlc
    xfce.gvfs
    xfce.thunar-bare
    xsel
  ]

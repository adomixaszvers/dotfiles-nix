{ pkgs, ... }:
with pkgs; [
  (callPackage ./consul {})
  (callPackage ./vimgolf {})
  (ghc.withPackages (
  haskellPackages: with haskellPackages; [
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
      ]))
      (python3.withPackages (
      pythonPackages: with pythonPackages; [
        i3ipc requests taskw psutil
      ]))
      arandr
      atom
      calibre
      discord
      exercism
      file
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
    # skype
    smartgithg
    stack
    thefuck
    torbrowser
    tree
    # typora
    vcsh
    viber
    vim
    vlc
    xfce.gvfs
    xfce.thunar-bare
  ]

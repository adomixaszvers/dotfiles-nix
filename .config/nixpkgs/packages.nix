{ pkgs, ... }:
with pkgs;
let
  haskellPackages = ps: with ps; [
        # ghc-mod
        hasktags
        hdevtools
        hindent
        hlint
        hoogle
        hspec
        pointfree pointful
        stylish-haskell
      ];
      ghcCustom = (ghc.withPackages haskellPackages);
      consul = (callPackage ./pkgs/consul {});
      vimgolf = (callPackage ./pkgs/vimgolf {});
      bumblebee-status = (callPackage ./pkgs/bumblebee-status {});
      config = { allowUnfree = true; };
      unstablePkgs = import (pkgs.fetchgit {
        url = "https://github.com/NixOS/nixpkgs-channels.git";
        rev = "b853e73d528385029960ef5bae18c278d0a32f94";
        # date = "2018-09-18T19:14:17-04:00";
        sha256 = "132jfbghzwc7h9kqvrk4nakp14v2ha8yq4i3rdalyxwa7qjw3c0h";
        fetchSubmodules = true;
      }) { inherit config; };
    in
    [
      unstablePkgs.skype
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

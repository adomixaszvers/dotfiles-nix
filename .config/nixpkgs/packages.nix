{ pkgs, ... }:
with pkgs;
let
  fetchNixPkgs = import ./fetchNixPkgs.nix;
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
      unstablePkgs = import (fetchNixPkgs {
        rev = "9fa6a261fb237f68071b361a9913ed1742d5e082";
        sha256 = "11733y8xfbisvp8jzpcpjwz70883qfnlzdxv7yl3k2accin88a9z";
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

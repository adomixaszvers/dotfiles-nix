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
      unstablePkgs = import (fetchNixPkgs {
        rev = "9fa6a261fb237f68071b361a9913ed1742d5e082";
        sha256 = "11733y8xfbisvp8jzpcpjwz70883qfnlzdxv7yl3k2accin88a9z";
      }) { inherit config; };
    in
    {
      all = [
        unstablePkgs.skype
        arandr
        bumblebee-status
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
        jdk8
        jetbrains.idea-ultimate
        keepass
        klavaro
        lxappearance
        maven
        meld
        ncdu
        nodejs
        notify-osd
        evince
        qbittorrent
        ranger
        rxvt_unicode-with-plugins
        unstablePkgs.smartgithg
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
      ];
      common = [
        arandr
        bumblebee-status
        file
        git
        gnome3.adwaita-icon-theme
        gnome3.gnome-screenshot
        google-chrome
        htop
        keepass
        meld
        ncdu
        notify-osd
        evince
        ranger
        rxvt_unicode-with-plugins
        thefuck
        tree
        vcsh
        xfce.gvfs
        xfce.thunar-bare
        xsel
      ];
      home = [
        discord
        exercism
        ghcCustom
        stack
        torbrowser
        vim
        vimgolf
        vlc
        qbittorrent
      ];
      work = [
        consul
        jdk8
        jetbrains.idea-ultimate
        libreoffice-fresh
        maven
        nodejs
        yarn
        unstablePkgs.smartgithg
        unstablePkgs.skype
      ];
    }

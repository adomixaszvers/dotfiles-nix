pkgs:
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
        unstablePkgs.exercism
        ghcCustom
        stack
        torbrowser
        vim
        vimgolf
        vlc
        qbittorrent
        unstablePkgs.typora
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

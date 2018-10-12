pkgs:
with pkgs;
let
  fonts = [
    corefonts
    dejavu_fonts
    source-code-pro
    google-fonts
    nerdfonts
  ];
in
  {
    common = fonts ++ [
      arandr
      dunst
      evince
      file
      git
      gnome3.adwaita-icon-theme
      gnome3.file-roller
      google-chrome
      htop
      i3lock
      keepass
      meld
      mine.bumblebee-status
      ncdu
      p7zip
      ranger
      rxvt_unicode-with-plugins
      shutter
      thefuck
      tree
      vcsh
      w3m # for ranger image previews
      xfce.gvfs
      xfce.thunar-bare
      xsel
    ];
    home = [
      discord
      exercism
      ghc
      stack
      # torbrowser
      vim
      mine.vimgolf
      vlc
      qbittorrent
      typora
    ];
    work = [
      docker
      visualvm
      mine.consul
      jdk8
      jetbrains.idea-ultimate
      libreoffice-fresh
      maven
      nodejs
      yarn
      smartgithg
      skype
    ];
  }

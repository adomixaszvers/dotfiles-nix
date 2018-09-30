pkgs:
with pkgs;
{
  common = [
    arandr
    mine.bumblebee-status
    file
    git
    gnome3.adwaita-icon-theme
    gnome3.gnome-screenshot
    google-chrome
    i3lock
    htop
    keepass
    meld
    ncdu
    lxqt.lxqt-notificationd
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
    ghc
    stack
    torbrowser
    vim
    mine.vimgolf
    vlc
    qbittorrent
    unstablePkgs.typora
  ];
  work = [
    mine.consul
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

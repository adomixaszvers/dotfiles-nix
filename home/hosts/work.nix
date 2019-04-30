{ pkgs, ... }:
{
  imports = [ ../emacs.nix ];
  home.packages = let unstable = import <nixos-unstable> {}; in with pkgs;
  [
    # mine.consul
    # yarn
    (lowPrio jdk11)
    (sqldeveloper_18.override { jdk = openjdk; })
    docker
    docker-compose
    docker-machine
    filezilla
    flameshot
    gnome3.nautilus
    google-chrome
    jdk8
    jetbrains.idea-ultimate
    jetbrains.datagrip
    jq
    libreoffice-fresh
    maven
    mercurial
    nodejs
    remmina
    robo3t
    samba
    skype
    smartgithg
    subversion
    soapui
    steam
    swagger-codegen
    traceroute
    postman
    unzip
    visualvm
    whois
  ];
  home.sessionVariables = {
    BROWSER = "${pkgs.google-chrome}/bin/google-chrome-stable";
  };
  services.screen-locker = {
    enable = true;
    inactiveInterval = 5;
    lockCmd = "i3lock -n";
  };
  xsession.windowManager.i3.config.startup = [
    # add services.flatpak.enable = true in OS configuration
    # flatpak --user remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
    # flatpak --user install flathub chat.rocket.RocketChat
    { command = "flatpak run chat.rocket.RocketChat"; notification = false; }
  ];
}

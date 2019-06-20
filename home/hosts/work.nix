{ pkgs, ... }: {
  home.packages = let unstable = import <nixos-unstable> { };
  in with pkgs; [
    # mine.consul
    # yarn
    (sqldeveloper_18.override { jdk = openjdk; })
    docker
    docker-compose
    docker-machine
    filezilla
    firefox
    flameshot
    gitAndTools.gitflow
    gnome3.nautilus
    google-chrome
    gradle
    jetbrains.datagrip
    jetbrains.idea-ultimate
    jq
    libreoffice-fresh
    liquibase
    maven
    mercurial
    nodejs
    remmina
    robo3t
    samba
    skype
    smartgithg
    soapui
    steam
    subversion
    swagger-codegen
    tomcat8
    traceroute
    unstable.postman
    unzip
    vagrant
    visualvm
    whois
  ];
  home.sessionVariables = {
    BROWSER = "firefox";
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
    {
      command = "flatpak run chat.rocket.RocketChat";
      notification = false;
    }
  ];
}

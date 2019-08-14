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
    unstable.jetbrains.idea-ultimate
    jq
    keepassxc
    libreoffice-fresh
    liquibase
    maven
    mercurial
    mine.steam
    nodejs
    rambox
    remmina
    robo3t
    samba
    soapui
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
  home.sessionVariables = { BROWSER = "firefox"; };
  programs.zsh.shellAliases = {
    imvn = "mvn -s ~/.m2/insoft-settings.xml";
    amvn = "mvn -s ~/.m2/kazan-settings.xml";
  };
  services.polybar.config."module/temperature".thermal-zone = 2;
  services.screen-locker = {
    enable = true;
    inactiveInterval = 5;
    lockCmd = let wallpaper = ../dotfiles/lockscreen.png;
    in "i3lock -n -i ${wallpaper} -t -p win -f";
  };
  xsession.windowManager.i3.config.startup = [{
    command = "rambox";
    notification = false;
  }];
}

{ pkgs, unstable, ... }: {
  home.file."jdks/openjdk8".source = pkgs.openjdk8;
  home.file."jdks/oraclejdk8".source = unstable.oraclejdk8;
  home.file."jdks/openjdk11".source = pkgs.openjdk11;
  home.file."nodejs/latest".source = pkgs.nodejs_latest;
  home.file."nodejs/12".source = pkgs.nodejs-12_x;
  home.file."maven".source = pkgs.maven;
  home.file."tomcat-native".source = pkgs.tomcat-native;
  home.packages = with pkgs; [
    asciinema
    calibre
    docker-compose
    dos2unix
    evince
    filezilla
    firefox
    gimp
    gitAndTools.gitflow
    gnome.libsecret
    gnome.nautilus
    gnumake
    google-chrome
    html-tidy
    jetbrains.idea-ultimate
    jmeter
    jq
    keepassxc
    libreoffice-still
    liquibase
    maven
    mercurial
    numlockx
    oracle-instantclient
    playerctl
    postman
    rambox
    remmina
    samba
    soapui
    spotify
    sqldeveloper
    sshpass
    steam-run
    subversion
    teams
    traceroute
    unrar
    unstable.torbrowser
    unzip
    whois
    zip
    zoom-us
  ];
  home.sessionVariables = { BROWSER = "google-chrome-stable"; };
  programs.git.includes = [
    {
      condition = "gitdir:~/projektai/**";
      contents.core.excludesfile = "${./work/gitignore_global}";
    }
    {
      condition = "gitdir:~/projektai/**";
      path = "~/projektai/git_work.inc";
    }
  ];
  programs.xmobar.thermal-zone = 2;
  programs.zsh.shellAliases = {
    imvn = "mvn -s ~/.m2/insoft-settings.xml";
    amvn = "mvn -s ~/.m2/kazan-settings.xml";
  };
  services.network-manager-applet.enable = true;
  services.polybar.config."module/temperature".thermal-zone = 2;
  xsession.windowManager.i3.config.startup = [{
    command = "rambox";
    notification = false;
  }];
  xsession.windowManager.bspwm = {
    extraConfig = ''
      bspc desktop 3 -l monocle
    '';
    startupPrograms = [ "rambox" ];
  };
}

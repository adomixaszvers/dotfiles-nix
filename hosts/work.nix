{ pkgs, ... }: {
  home.packages = let
    unstable = pkgs.channels.nixos-unstable;
    myEclipse = with pkgs.eclipses;
      eclipseWithPlugins {
        eclipse = eclipse-java;
        plugins = [
          (plugins.buildEclipseUpdateSite {
            name = "activiti-designer-5.18.0";
            src = pkgs.fetchzip {
              stripRoot = false;
              url =
                "http://www.activiti.org/designer/archived/activiti-designer-5.18.0.zip";
              sha256 = "1iimskpdvibq1z11hh48krq2qvw6qhddl41qbqc7547x3g19slfr";
            };
          })
        ];
      };
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
    gnumake
    google-chrome
    gradle
    jq
    keepassxc
    libreoffice-fresh
    liquibase
    maven
    mercurial
    mine.dbxcli
    mine.steam
    myEclipse
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
    unrar
    unstable.jetbrains.idea-ultimate
    unstable.postman
    unzip
    vagrant
    visualvm
    whois
  ];
  home.sessionVariables = { BROWSER = "firefox"; };
  programs.autorandr = {
    enable = true;
    profiles = {
      work = {
        fingerprint = {
          DP-1 =
            "00ffffffffffff0022f06e32010101012b1a0104a5342078224ca5a7554da226105054210800b30095008100d1c0a9c081c0a9408180283c80a070b023403020360006442100001a000000fd00323c1e5011010a202020202020000000fc00485020453234320a2020202020000000ff00434e43363433303832370a20200019";
          DP-2 =
            "00ffffffffffff0022f06e32010101010e1a0104a5342078224ca5a7554da226105054210800b30095008100d1c0a9c081c0a9408180283c80a070b023403020360006442100001a000000fd00323c1e5011010a202020202020000000fc00485020453234320a2020202020000000ff00434e433631343036364d0a20200020";
        };
        config = {
          HDMI1.enable = false;
          HDMI2.enable = false;
          VGA1.enable = false;
          VIRTUAL1.enable = false;
          DP-1 = {
            enable = true;
            position = "0x0";
            rate = "59.95";
            mode = "1920x1200";
          };
          DP-2 = {
            enable = true;
            position = "1920x0";
            primary = true;
            rate = "59.95";
            mode = "1920x1200";
          };
        };
        hooks.postswitch = "systemctl --user restart compton.service";
      };
    };
  };
  programs.zsh.shellAliases = {
    imvn = "mvn -s ~/.m2/insoft-settings.xml";
    amvn = "mvn -s ~/.m2/kazan-settings.xml";
  };
  services.polybar.config."module/temperature".thermal-zone = 2;
  services.screen-locker = {
    enable = true;
    inactiveInterval = 5;
    lockCmd = let wallpaper = ./lockscreen.png;
    in "i3lock -n -i ${wallpaper} -t -p win -f";
  };
  xsession.windowManager.i3.config.startup = [{
    command = "rambox";
    notification = false;
  }];
  xsession.initExtra = ''
    autorandr --change
  '';
}

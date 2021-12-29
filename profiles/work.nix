{ pkgs, unstable, ... }: {
  imports = [ ./common.nix ./wm/xsession-common.nix ./wm/xmonad ];
  colors = import ./gui/colors/nord.nix;
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
  programs.autorandr = {
    enable = true;
    profiles = {
      work = {
        fingerprint = {
          DP-1 =
            "00ffffffffffff0022f06e32010101010e1a0104a5342078224ca5a7554da226105054210800b30095008100d1c0a9c081c0a9408180283c80a070b023403020360006442100001a000000fd00323c1e5011010a202020202020000000fc00485020453234320a2020202020000000ff00434e433631343036364d0a20200020";
          DP-2 =
            "00ffffffffffff0022f06e32010101012b1a0104a5342078224ca5a7554da226105054210800b30095008100d1c0a9c081c0a9408180283c80a070b023403020360006442100001a000000fd00323c1e5011010a202020202020000000fc00485020453234320a2020202020000000ff00434e43363433303832370a20200019";
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
      };
    };
  };
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
  services.screen-locker = {
    enable = true;
    inactiveInterval = 5;
    lockCmd = "${pkgs.xsecurelock}/bin/xsecurelock";
    xss-lock.extraOptions = [ "-n ${pkgs.xsecurelock}/libexec/dimmer" ];
  };
  services.syncthing = { enable = true; };
  xsession.windowManager.i3.config.startup = [{
    command = "rambox";
    notification = false;
  }];
  xsession.windowManager.bspwm = {
    extraConfig = ''
      bspc desktop 3 -l monocle
    '';
    monitors = {
      "DP-1" = [ "1" "2" "3" "4" "5" ];
      "DP-2" = [ "6" "7" "8" "9" "10" ];
    };
    startupPrograms = [ "rambox" ];
  };
  xsession.initExtra = ''
    autorandr --change
  '';
  wayland.windowManager.sway.config = {
    output = {
      DP-1 = { pos = "0 0"; };
      DP-2 = { pos = "1920 0"; };
    };
    input = {
      "6127:24729:Lenovo_Lenovo_Traditional_USB_Keyboard" = {
        xkb_layout = "lt,us";
        xkb_numlock = "enabled";
      };
    };
  };
}

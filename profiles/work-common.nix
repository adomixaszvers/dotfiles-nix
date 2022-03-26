{ pkgs, unstable, ... }: {
  imports = [ ./common.nix ./wm/xsession-common.nix ./wm/xmonad ];
  home.file."jdks/openjdk8".source = pkgs.openjdk8;
  home.file."jdks/oraclejdk8".source = unstable.oraclejdk8;
  home.file."jdks/openjdk11".source = pkgs.openjdk11;
  home.file."nodejs/latest".source = pkgs.nodejs_latest;
  home.file."nodejs/12".source = pkgs.nodejs-12_x;
  home.file."maven".source = pkgs.maven;
  home.file."tomcat-native".source = pkgs.tomcat-native;
  home.packages = with pkgs; [
    asciinema
    brightnessctl
    docker-compose
    dos2unix
    evince
    filezilla
    firefox
    gimp
    gitAndTools.gitflow
    libsecret
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
    protonup
    soapui
    spotify
    sqldeveloper
    sshpass
    steam-run
    subversion
    teams
    traceroute
    unrar
    unstable.discord
    unstable.tor-browser-bundle-bin
    unzip
    whois
    zip
    zoom-us
  ];

  services.polybar.config = {
    "bar/top" = {
      modules-right =
        "battery divider memory divider disk divider cpu divider temperature divider volume divider keyboard divider date divider time divider";
    };
    "module/battery" = {
      type = "internal/battery";
      battery = "BAT0";
      adapter = "AC";
      format-charging = "<ramp-capacity> <label-charging>";
      format-discharging = "<ramp-capacity> <label-discharging>";
      label-charging = " c %percentage%%";
      label-discharging = " d %percentage%%";

      ramp-capacity-0 = "";
      ramp-capacity-1 = "";
      ramp-capacity-2 = "";
      ramp-capacity-3 = "";
      ramp-capacity-4 = "";
    };
  };

  programs.autorandr = {
    hooks.postswitch.restart-picom = "systemctl --user restart picom.service";
    profiles = {
      work-single = {
        fingerprint = {
          eDP-1 =
            "00ffffffffffff000daee71500000000211a0104a52213780228659759548e271e505400000001010101010101010101010101010101b43b804a713834405036680058c110000018000000fe004e3135364843412d4541420a20000000fe00434d4e0a202020202020202020000000fe004e3135364843412d4541420a2000b2";
        };
        config = {
          eDP-1 = {
            enable = true;
            crtc = 0;
            position = "0x0";
            primary = true;
            rate = "60.01";
            mode = "1920x1080";
          };
        };
      };
      work = {
        fingerprint = {
          DP-2-2 =
            "00ffffffffffff0022f06e32010101010e1a0104a5342078224ca5a7554da226105054210800b30095008100d1c0a9c081c0a9408180283c80a070b023403020360006442100001a000000fd00323c1e5011010a202020202020000000fc00485020453234320a2020202020000000ff00434e433631343036364d0a20200020";
          DP-2-3 =
            "00ffffffffffff0022f06e32010101012b1a0104a5342078224ca5a7554da226105054210800b30095008100d1c0a9c081c0a9408180283c80a070b023403020360006442100001a000000fd00323c1e5011010a202020202020000000fc00485020453234320a2020202020000000ff00434e43363433303832370a20200019";
          eDP-1 =
            "00ffffffffffff000daee71500000000211a0104a52213780228659759548e271e505400000001010101010101010101010101010101b43b804a713834405036680058c110000018000000fe004e3135364843412d4541420a20000000fe00434d4e0a202020202020202020000000fe004e3135364843412d4541420a2000b2";
        };
        config = {
          DP-2-2 = {
            enable = true;
            crtc = 2;
            position = "0x0";
            rate = "59.95";
            mode = "1920x1200";
          };
          DP-2-3 = {
            primary = true;
            enable = true;
            crtc = 0;
            position = "1920x0";
            rate = "59.95";
            mode = "1920x1200";
          };
          eDP-1 = {
            enable = true;
            crtc = 1;
            mode = "1920x1080";
            position = "3840x0";
            rate = "60.01";
          };
        };
      };
      work-prime = {
        fingerprint = {
          DP-3-2 =
            "00ffffffffffff0022f06e32010101010e1a0104a5342078224ca5a7554da226105054210800b30095008100d1c0a9c081c0a9408180283c80a070b023403020360006442100001a000000fd00323c1e5011010a202020202020000000fc00485020453234320a2020202020000000ff00434e433631343036364d0a20200020";
          DP-3-3 =
            "00ffffffffffff0022f06e32010101012b1a0104a5342078224ca5a7554da226105054210800b30095008100d1c0a9c081c0a9408180283c80a070b023403020360006442100001a000000fd00323c1e5011010a202020202020000000fc00485020453234320a2020202020000000ff00434e43363433303832370a20200019";
          eDP-1 =
            "00ffffffffffff000daee71500000000211a0104a52213780228659759548e271e505400000001010101010101010101010101010101b43b804a713834405036680058c110000018000000fe004e3135364843412d4541420a20000000fe00434d4e0a202020202020202020000000fe004e3135364843412d4541420a2000b2";
        };
        config = {
          DP-3-2 = {
            enable = true;
            crtc = 2;
            position = "0x0";
            rate = "59.95";
            mode = "1920x1200";
          };
          DP-3-3 = {
            primary = true;
            enable = true;
            crtc = 0;
            position = "1920x0";
            rate = "59.95";
            mode = "1920x1200";
          };
          eDP-1 = {
            enable = true;
            crtc = 1;
            mode = "1920x1080";
            position = "3840x0";
            rate = "60.01";
          };
        };
      };
    };
  };

  services.kanshi.profiles = {
    docked.outputs = [
      {
        criteria = "Hewlett Packard HP E242 CNC614066M";
        position = "0,0";
      }
      {
        criteria = "Hewlett Packard HP E242 CNC6430827";
        position = "1920,0";
      }
      {
        criteria = "Chimei Innolux Corporation 0x15E7 0x00000000";
        position = "3840,0";
      }
    ];
    undocked.outputs = [{
      criteria = "Chimei Innolux Corporation 0x15E7 0x00000000";
      position = "0,0";
    }];
  };

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
  programs.xmobar.thermal-zone = 1;
  programs.zsh.shellAliases = {
    imvn = "mvn -s ~/.m2/insoft-settings.xml";
    amvn = "mvn -s ~/.m2/kazan-settings.xml";
  };
  services.network-manager-applet.enable = true;
  services.polybar.config."module/temperature".thermal-zone = 1;
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
  wayland.windowManager.sway.config = {
    input = {
      "6127:24729:Lenovo_Lenovo_Traditional_USB_Keyboard" = {
        xkb_layout = "lt,us";
        xkb_numlock = "enabled";
      };
      "1:1:AT_Translated_Set_2_keyboard" = {
        xkb_layout = "lt,us";
        xkb_numlock = "enabled";
      };
    };
  };
}

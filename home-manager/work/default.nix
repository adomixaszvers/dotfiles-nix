{
  pkgs,
  lib,
  myPkgs,
  config,
  ...
}:
{
  imports = [
    ../common.nix
    ../cli/atuin
    ../cli/jujutsu.nix
    ../wm/xrdp.nix
    ./kerberos.nix
  ];
  specialisation = {
    work.configuration = {
      imports = [ ./work.nix ];
      xdg.dataFile."home-manager/specialisation".text = "work";
    };

    work-remote.configuration = {
      imports = [ ./work-remote.nix ];
      xdg.dataFile."home-manager/specialisation".text = "work-remote";
    };
  };
  gui = {
    hasBattery = true;
    thermal-zone = 1;
  };
  stylix.enable = true;
  xdg.dataFile."dbus-1/services/org.freedesktop.secrets.service".text = ''
    [D-BUS Service]
    Name=org.freedesktop.secrets
    Exec=${lib.getExe pkgs.keepassxc}
  '';
  home = {
    file = {
      "jdks/openjdk8".source = pkgs.openjdk8;
      "jdks/openjdk11".source = pkgs.openjdk11;
      "jdks/openjdk17".source = pkgs.openjdk17;
      "jdks/openjdk21".source = pkgs.openjdk21;
      "jdks/openjdk25".source = pkgs.openjdk25;
      "nodejs/24".source = pkgs.nodejs_24;
      "maven".source = pkgs.maven;
      "tomcat-native".source = pkgs.tomcat-native;
      "tomcats/tomcat9".source = pkgs.tomcat9;
      "pythons/python3".source = pkgs.python3.withPackages (
        ps: with ps; [
          cython
          setuptools
        ]
      );
    };
    packages = with pkgs; [
      # keep-sorted start block=yes
      asciinema
      brave
      brightnessctl
      dbeaver-bin
      docker-credential-helpers
      dos2unix
      dumpasn1
      evince
      filezilla
      ghex
      gimp
      gitflow
      gnumake
      html-tidy
      jetbrains.idea
      jmeter
      jq
      keepassxc
      keystore-explorer
      libreoffice-still
      libsecret
      liquibase
      maven
      mercurial
      minio-client
      myPkgs.hunspell-lt
      myPkgs.soapui
      nemo
      numlockx
      openssl
      playerctl
      pnpm_11
      podman-compose
      protonup-ng
      # volatile versions
      # postman
      remmina
      rlwrap
      samba
      sshpass
      steam-run
      subversion
      thunderbird
      tor-browser
      # teams
      traceroute
      unrar
      unzip
      whois
      xpra
      zip
      # keep-sorted end
    ];
    sessionPath = [ config.home.sessionVariables.PNPM_HOME ];
    sessionVariables = {
      BROWSER = "firefox";
      PNPM_HOME = "${config.xdg.dataHome}/pnpm";
      # see https://github.com/skywind3000/z.lua?tab=readme-ov-file#options
      TESTCONTAINERS_RYUK_DISABLED = "true";
      _ZL_EXCLUDE_DIRS = "/kiti"; # don't trigger automount with z-lua
    };
  };

  programs = {
    git.includes = [
      {
        condition = "gitdir:~/projektai/**";
        contents.core.excludesfile = "${./gitignore_global}";
      }
      {
        condition = "gitdir:~/projektai/**";
        path = "~/projektai/git_work.inc";
      }
    ];
    gh = {
      enable = true;
      gitCredentialHelper.enable = true;
    };
    # see https://learn.microsoft.com/en-us/troubleshoot/microsoftteams/teams-sign-in/sign-in-loop
    firefox.policies.EnableTrackingProtection.Exceptions = [
      "https://teams.cloud.microsoft/"
      "https://outlook.office.com"
    ];
    jujutsu.settings = {
      aliases.jr = [
        "util"
        "exec"
        "--"
        (lib.getExe myPkgs.jj-jr)
      ];
      merge-tools.idea = {
        program = "idea";
        diff-args = [
          "diff"
          "$left"
          "$right"
        ];
        edit-args = [
          "diff"
          "$left"
          "$right"
        ];
        merge-args = [
          "merge"
          "$left"
          "$right"
          "$base"
          "$output"
        ];
      };
    };
  };

  services = {
    gnome-keyring.enable = lib.mkForce false;
    shikane = {
      settings.profile = [
        {
          name = "work-duo";
          output = [
            {
              adaptive_sync = true;
              enable = true;
              mode = "2560x1440@59.951Hz";
              position = "0,0";
              scale = 1;
              search = [
                "m=DELL P2723D"
                "s=3MHX0V3"
                "v=Dell Inc."
              ];
              transform = "normal";
            }
            {
              adaptive_sync = true;
              enable = true;
              mode = "2560x1440@59.951Hz";
              position = "2560,0";
              scale = 1;
              search = [
                "m=DELL P2723D"
                "s=GRJX0V3"
                "v=Dell Inc."
              ];
              transform = "normal";
            }
            {
              enable = false;
              search = [
                "m=0x157F"
                "s="
                "v=Sharp Corporation"
              ];
            }
          ];
        }
        {
          name = "undocked";
          output = [
            {
              adaptive_sync = false;
              enable = true;
              position = "0,0";
              mode = "1920x1200@120.003Hz";
              scale = 1;
              search = [
                "m=0x157F"
                "s="
                "v=Sharp Corporation"
              ];
              transform = "normal";
            }
          ];
        }
      ];
    };
    # network-manager-applet.enable = true;
    polybar.config = {
      "module/temperature".thermal-zone = config.gui.thermal-zone;
      "bar/top" = {
        modules-right = "battery disk memory cpu temperature volume keyboard date time tray";
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
  };

  xsession.windowManager.bspwm = {
    extraConfig = # bash
      ''
        bspc desktop 3 -l monocle
      '';
  };
  xdg = {
    configFile."mimeapps.list".force = true;
    mimeApps = {
      enable = true;
      defaultApplications = {
        "x-scheme-handler/https" = [
          "firefox.desktop"
          "google-chrome.desktop"
        ];
        "x-scheme-handler/http" = [
          "firefox.desktop"
          "google-chrome.desktop"
        ];
        "text/html" = [
          "firefox.desktop"
          "google-chrome.desktop"
        ];
        "inode/directory" = "org.gnome.Nautilus.desktop";
      };
    };
  };
  wayland.windowManager = {
    sway.config = {
      input = {
        "6127:24729:Lenovo_Lenovo_Traditional_USB_Keyboard" = {
          xkb_layout = "lt,us";
          xkb_numlock = "enabled";
          xkb_options = "grp:caps_toggle";
        };
        "1:1:AT_Translated_Set_2_keyboard" = {
          xkb_layout = "lt,us";
          xkb_numlock = "enabled";
          xkb_options = "grp:caps_toggle";
        };
      };
    };
  };
}

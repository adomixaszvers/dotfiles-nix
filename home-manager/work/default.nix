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
    ../wm/niri
    ../wm/xmonad
    ../wm/xinitrc.nix
    ./kerberos.nix
    ./legacy.nix
  ];
  gui = {
    hasBattery = true;
    thermal-zone = 1;
  };
  stylix.enable = true;
  xdg = {
    dataFile."dbus-1/services/org.freedesktop.secrets.service".text = ''
      [D-BUS Service]
      Name=org.freedesktop.secrets
      Exec=${lib.getExe pkgs.keepassxc}
    '';

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
      "pythons/python3".source = pkgs.python3.withPackages (ps: [
        ps.cython
        ps.setuptools
      ]);
    };
    packages = builtins.attrValues {
      inherit (pkgs)
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
        jmeter
        jq
        keepassxc
        keystore-explorer
        libreoffice-stable
        libsecret
        liquibase
        maven
        mercurial
        minio-client
        nemo
        numlockx
        openssl
        playerctl
        pnpm_12
        podman-compose
        protonup-ng
        # volatile versions
        # postman
        remmina
        rlwrap
        samba
        soapui
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
        zip
        # keep-sorted end
        ;
      inherit (myPkgs) hunspell-lt;
      inherit (pkgs.jetbrains) idea;
    };
    sessionPath = [ config.home.sessionVariables.PNPM_HOME ];
    sessionVariables = {
      BROWSER = "firefox";
      PNPM_HOME = "${config.xdg.dataHome}/pnpm";
      # see https://github.com/skywind3000/z.lua?tab=readme-ov-file#options
      TESTCONTAINERS_RYUK_DISABLED = "true";
      _ZL_EXCLUDE_DIRS = "/kiti"; # don't trigger automount with z-lua
    };
    sessionVariables.KITTY_CONF_FONT = "font_size 9";
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
        program = "intellij-idea";
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

    hyprlock = {
      enable = true;
      settings = {
        animations.enabled = false;
        general = {
          ignore_empty_input = true;
        };
        auth.fingerprint.enabled = true;
        label = [
          {
            monitor = "";
            position = "0, 100";
            text = "$TIME";
            font_size = 40;
            color = "rgb(${config.lib.stylix.colors.base05})";
            halign = "center";
            valign = "center";
          }
          {
            monitor = "";
            text = "$FPRINTPROMPT";
            text_align = "center";
            color = "rgb(${config.lib.stylix.colors.base05})";
            font_size = 24;
            position = "0, -100";
            halign = "center";
            valign = "center";
          }
        ];
      };
    };
    noctalia.settings = {
      lockscreen.enabled = true;
      idle = {
        behavior = {
          lock = {
            action = "lock";
            enabled = true;
            timeout = 300;
          };
          screen-off = {
            action = "screen_off";
            enabled = true;
            timeout = 330;
          };
        };
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
    network-manager-applet.enable = true;
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

    shikane.enable = true;
    picom.enable = false;
    hypridle = {
      enable = false;
      settings =
        let
          isHypr = config.wayland.windowManager.hyprland.enable;
          isNiri = config.wayland.windowManager.niri.enable;
          niri = lib.getExe config.wayland.windowManager.niri.package;
          dpmsOnCmd =
            if isHypr then
              "hyprctl dispatch dpms on"
            else if isNiri then
              "${niri} msg action power-on-monitors"
            else
              "";
          dpmsOffCmd =
            if isHypr then
              "hyprctl dispatch dpms off"
            else if isNiri then
              "${niri} msg action power-off-monitors"
            else
              "";
        in
        {
          general = {
            before_sleep_cmd = "loginctl lock-session";
            after_sleep_cmd = dpmsOnCmd;
            lock_cmd = "pidof hyprlock || hyprlock";
          };
          listener = [
            {
              timeout = 300;
              on-timeout = "loginctl lock-session";
            }
            {
              timeout = 330;
              on-timeout = "${dpmsOffCmd}";
              on-resume = dpmsOnCmd;
            }
          ];
        };
    };
  };
  wayland.windowManager =
    let
      ideaOptions = pkgs.writeText "idea64.vmoptions" ''
        -Xmx3971m
        -Dawt.toolkit.name=WLToolkit
      '';
    in
    {
      hyprland.settings = {
        env = [
          {
            _args = [
              "IDEA_VM_OPTIONS"
              ideaOptions
            ];
          }
        ];
        monitor = [
          {
            output = "DP-6";
            mode = "2560x1440";
            position = "0x0";
            scale = "1.0";
          }
          {
            output = "DP-7";
            mode = "2560x1440";
            position = "2560x0";
            scale = "1.0";
          }
          # "eDP-1,disable"
        ];
      };
      niri.settings = {
        environment = {
          IDEA_VM_OPTIONS = ideaOptions.outPath;
        };
        input.touchpad.tap = { };
        _children = [
          {
            workspace = {
              _args = [
                "browser"
              ];
              open-on-output = "Dell Inc. DELL P2723D GRJX0V3";
            };
          }
          {
            workspace = {
              _args = [
                "dev"
              ];
              open-on-output = "Dell Inc. DELL P2723D 3MHX0V3";
            };
          }
        ];
      };
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
        startup = [
          {
            command = "${lib.getExe pkgs.swayidle} -w timeout 300 '${pkgs.swaylock-effects}/bin/swaylock --clock --screenshots --effect-blur 7x5 --effect-vignette 0.5:0.5'";
          }
        ];
        output = {
          "Hewlett Packard HP E242 CNC614066M" = {
            pos = "0 0";
          };
          "Hewlett Packard HP E242 CNC6430827" = {
            pos = "1920 0";
          };
        };
      };
    };
  xsession = {
    enable = lib.mkDefault true;
    importedVariables = [ "XDG_SESSION_TYPE" ];
    numlock.enable = true;
    windowManager.bspwm = {
      monitors = {
        "DP-1" = [
          "1"
          "2"
          "3"
          "4"
          "5"
        ];
        "DP-2" = [
          "6"
          "7"
          "8"
          "9"
          "10"
        ];
      };
    };
  };
  systemd.user.services = {
    dunst.Unit.ConditionEnvironment = "XDG_SESSION_TYPE=x11";
    network-manager-applet.Unit.ConditionEnvironment = "XDG_SESSION_TYPE=x11";
    noctalia.Unit.ConditionEnvironment = "WAYLAND_DISPLAY";
  };

}

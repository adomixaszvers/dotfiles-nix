{
  pkgs,
  lib,
  myPkgs,
  config,
  ...
}:
{
  imports = [
    ./common.nix
    ./cli/atuin
    ./wm/xrdp.nix
    ./work/kerberos.nix
    ./cli/jujutsu.nix
  ];
  gui = {
    hasBattery = true;
    thermal-zone = 1;
  };
  stylix.enable = true;
  xdg.dataFile."dbus-1/services/org.freedesktop.secrets.service".text = ''
    [D-BUS Service]
    Name=org.freedesktop.secrets
    Exec=${pkgs.keepassxc}/bin/keepassxc
  '';
  home = {
    file = {
      "jdks/openjdk8".source = pkgs.openjdk8;
      "jdks/oraclejdk8".source = myPkgs.oraclejdk8;
      "jdks/openjdk11".source = pkgs.openjdk11;
      "jdks/openjdk17".source = pkgs.openjdk17;
      "jdks/openjdk21".source = pkgs.openjdk21;
      "nodejs/12".source =
        let
          # nixos-20.09
          oldNixpkgs = builtins.getFlake "github:NixOS/nixpkgs/1c1f5649bb9c1b0d98637c8c365228f57126f361";
          oldPkgs = builtins.getAttr pkgs.stdenv.hostPlatform.system oldNixpkgs.legacyPackages;
        in
        oldPkgs.nodejs-12_x;
      "nodejs/22".source = pkgs.nodejs_22;
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
      asciinema
      ghex
      brave
      brightnessctl
      dbeaver-bin
      docker-credential-helpers
      dos2unix
      myPkgs.eclipse-activiti
      evince
      filezilla
      gimp
      gitflow
      libsecret
      nemo
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
      minio-client
      myPkgs.hunspell-lt
      keystore-explorer
      myPkgs.sqldeveloper
      mercurial
      pnpm_10
      podman-compose
      numlockx
      openssl
      oracle-instantclient
      playerctl
      # volatile versions
      # postman
      remmina
      rlwrap
      samba
      protonup-ng
      soapui
      # spotify
      sshpass
      steam-run
      subversion
      # teams
      traceroute
      thunderbird
      unrar
      discord
      tor-browser
      unzip
      whois
      xpra
      zip
      zoom-us
      dumpasn1
    ];
    sessionPath = [ config.home.sessionVariables.PNPM_HOME ];
    sessionVariables = {
      BROWSER = "firefox";
      CS_AUTH_KEYS = "/home/adomas/HSMrsa.pub";
      CS_PKCS11_R2_CFG = "/home/adomas/cs_pkcs11_R2.cfg";
      CRYPTOSERVER = "3001@localhost";
      PNPM_HOME = "${config.xdg.dataHome}/pnpm";
      # see https://github.com/skywind3000/z.lua?tab=readme-ov-file#options
      _ZL_EXCLUDE_DIRS = "/kiti"; # don't trigger automount with z-lua
    };
  };

  programs = {
    autorandr = {
      hooks = {
        preswitch.set-dpi = # bash
          ''
            if [ "$AUTORANDR_CURRENT_PROFILE" = work-single ]; then
              DPI=120
            else
              DPI=96
            fi
            echo "Xft.dpi: $DPI"| ${pkgs.xorg.xrdb}/bin/xrdb -merge
          '';
        postswitch = {
          restart-picom = "systemctl --user restart picom.service";
          notify = # bash
            ''${pkgs.libnotify}/bin/notify-send -i display "Display profile" "$AUTORANDR_CURRENT_PROFILE"'';
        };
      };
      profiles =
        let
          fingerprints = rec {
            eDP-1 = "00ffffffffffff000daee71500000000211a0104a52213780228659759548e271e505400000001010101010101010101010101010101b43b804a713834405036680058c110000018000000fe004e3135364843412d4541420a20000000fe00434d4e0a202020202020202020000000fe004e3135364843412d4541420a2000b2";
            DP-2-2 = "00ffffffffffff0010ac41d1424c563005210104a53c22783ad735ab534f9e250f5054a54b008100b300d100714fa9408180d1c00101565e00a0a0a029503020350055502100001a000000ff00334d48583056330a2020202020000000fc0044454c4c205032373233440a20000000fd00314b1d711c010a202020202020019702030eb14990040302011211131f023a801871382d40582c450055502100001e011d007251d01e206e28550055502100001e7e3900a080381f4030203a0055502100001a0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000d7";
            DP-2-3 = "00ffffffffffff0010ac41d1424b513005210104a53c22783ad735ab534f9e250f5054a54b008100b300d100714fa9408180d1c00101565e00a0a0a029503020350055502100001a000000ff0047524a583056330a2020202020000000fc0044454c4c205032373233440a20000000fd00314b1d711c010a202020202020018202030eb14990040302011211131f023a801871382d40582c450055502100001e011d007251d01e206e28550055502100001e7e3900a080381f4030203a0055502100001a0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000d7";
            DP-3-2 = DP-2-2;
            DP-3-3 = DP-2-3;
          };
        in
        {
          work-single = {
            fingerprint = {
              inherit (fingerprints) eDP-1;
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
          work-duo = {
            fingerprint = {
              inherit (fingerprints) DP-2-2 DP-2-3;
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
                position = "2560x0";
                rate = "59.95";
                mode = "2560x1440";
              };
              eDP-1 = {
                enable = false;
                crtc = 1;
                mode = "1920x1080";
                position = "3840x0";
                rate = "60.01";
              };
            };
          };
          work-duo-prime = {
            fingerprint = {
              inherit (fingerprints) DP-3-2 DP-3-3;
            };
            config = {
              DP-3-2 = {
                enable = true;
                crtc = 0;
                position = "0x0";
                rate = "59.95";
                mode = "2560x1440";
              };
              DP-3-3 = {
                enable = true;
                primary = true;
                crtc = 2;
                position = "2560x0";
                rate = "59.95";
                mode = "2560x1440";
              };
              eDP-1 = {
                enable = false;
                crtc = 1;
                mode = "1920x1080";
                position = "3840x0";
                rate = "60.01";
              };
            };
          };
          work-trio = {
            fingerprint = {
              inherit (fingerprints) eDP-1 DP-2-2 DP-2-3;
            };
            config = {
              eDP-1 = {
                enable = true;
                crtc = 1;
                mode = "1920x1080";
                position = "0x0";
                rate = "60.01";
              };
              DP-2-2 = {
                enable = true;
                primary = true;
                crtc = 2;
                position = "1920x0";
                rate = "59.95";
                mode = "2560x1440";
              };
              DP-2-3 = {
                enable = true;
                crtc = 0;
                position = "4480x0";
                rate = "59.95";
                mode = "2560x1440";
              };
            };
          };
          work-trio-prime = {
            fingerprint = {
              inherit (fingerprints) eDP-1 DP-3-2 DP-3-3;
            };
            config = {
              eDP-1 = {
                enable = true;
                crtc = 1;
                mode = "1920x1080";
                position = "5120x0";
                rate = "60.01";
              };
              DP-3-2 = {
                enable = true;
                crtc = 0;
                position = "0x0";
                rate = "59.95";
                mode = "2560x1440";
              };
              DP-3-3 = {
                enable = true;
                primary = true;
                crtc = 2;
                position = "2560x0";
                rate = "59.95";
                mode = "2560x1440";
              };
            };
          };
        };
    };
    git.includes = [
      {
        condition = "gitdir:~/projektai/**";
        contents.core.excludesfile = "${./work/gitignore_global}";
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
      "https://teams.microsoft.com"
      "https://outlook.office.com"
    ];
    jujutsu.settings = {
      merge-tools.idea = {
        program = "idea-ultimate";
        merge-args = [
          "merge"
          "$left"
          "$right"
          "$base"
          "$output"
        ];
      };
    };
    zsh.shellAliases = {
      imvn = "mvn -s ~/.m2/insoft-settings.xml";
      amvn = "mvn -s ~/.m2/kazan-settings.xml";
    };
  };

  services = {
    gnome-keyring.enable = lib.mkForce false;
    kanshi.profiles = {
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
          status = "disable";
        }
      ];
      undocked.outputs = [
        {
          criteria = "Chimei Innolux Corporation 0x15E7 0x00000000";
          position = "0,0";
          # scale = 1.25;
        }
      ];
    };
    shikane.settings.profile = [
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
              "m=0x15E7"
              "s="
              "v=Chimei Innolux Corporation"
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
            mode = "1920x1080@60";
            scale = 1;
            search = [
              "m=0x15E7"
              "s="
              "v=Chimei Innolux Corporation"
            ];
            transform = "normal";
          }
        ];
      }
    ];
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
    kbdd.enable = lib.mkDefault true;
  };
  xsession.windowManager.i3.config.startup = [
    {
      command = "rambox";
      notification = false;
    }
  ];

  xsession.windowManager.bspwm = {
    extraConfig = # bash
      ''
        bspc desktop 3 -l monocle
      '';
  };
  xdg = {
    autostart = {
      enable = false;
      entries = [ "${pkgs.keepassxc}/share/applications/org.keepassxc.KeePassXC.desktop" ];
      readOnly = false;
    };
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
    hyprland = {
      systemd.enableXdgAutostart = true;
      settings = {
        env =
          let
            ideaOptions = pkgs.writeText "idea64.vmoptions" ''
              -Xmx3971m
              -Dawt.toolkit.name=WLToolkit
            '';
          in
          [
            "IDEA_VM_OPTIONS,${ideaOptions}"
          ];
      };
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
    };
  };
}

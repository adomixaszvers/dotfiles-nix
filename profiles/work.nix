{
  pkgs,
  config,
  lib,
  ...
}:
{
  imports = [
    ./work-common.nix
    # ./wm/xsession-common.nix
    # ./wm/xmonad
    ./wm/hyprland
  ];
  # gtk.enable = false;
  # qt.enable = false;
  services = {
    # network-manager-applet.enable = false;
    # udiskie.enable = false;
    # kbdd.enable = false;
    # picom.enable = false;
    screen-locker = {
      enable = lib.mkDefault config.xsession.enable;
      inactiveInterval = 5;
    };
    hypridle = {
      enable = lib.mkDefault config.wayland.windowManager.hyprland.enable;
      settings = {
        general = {
          before_sleep_cmd = "loginctl lock-session";
          after_sleep_cmd = "hyprctl dispatch dpms on";
          lock_cmd = "pidof hyprlock || hyprlock";
        };
        listener = [
          {
            timeout = 300;
            on-timeout = "loginctl lock-session";
          }
          {
            timeout = 330;
            on-timeout = "hyprctl dispatch dpms off";
            on-resume = "hyprctl dispatch dpms on";
          }
        ];
      };
    };

  };
  programs = {
    hyprlock = {
      enable = lib.mkDefault config.wayland.windowManager.hyprland.enable;
      settings = {
        general = {
          enable_fingerprint = true;
        };
        background = {
          path = "screenshot";
          color = "rgba(25, 20, 20, 1.0)";
          blur_passes = 2;
        };
        input-field = [
          {
            size = "200, 50";
            position = "0, -80";
            monitor = "";
            dots_center = true;
            fade_on_empty = false;
            font_color = "rgb(202, 211, 245)";
            inner_color = "rgb(91, 96, 120)";
            outer_color = "rgb(24, 25, 38)";
            outline_thickness = 5;
            # placeholder_text = # html
            #   ''<span foreground="##cad3f5">Password...</span>'';
            shadow_passes = 2;
          }
        ];
      };
    };
  };
  xsession.windowManager.bspwm = {
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
  wayland.windowManager = {
    hyprland.settings.monitor = [
      "DP-6,2560x1440,0x0,1.00"
      "DP-7,2560x1440,2560x0,1.00"
      "eDP-1,1920x1080,5120x0,1.25"
    ];
    sway.config = {
      startup = [
        {
          command = "${pkgs.swayidle}/bin/swayidle -w timeout 300 '${pkgs.swaylock-effects}/bin/swaylock --clock --screenshots --effect-blur 7x5 --effect-vignette 0.5:0.5'";
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
}

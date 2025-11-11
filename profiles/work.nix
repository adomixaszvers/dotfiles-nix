{
  pkgs,
  config,
  lib,
  ...
}:
let
  isX11 = config.xsession.enable;
  isHypr = config.wayland.windowManager.hyprland.enable || config.programs.niri.enable;
in
{
  imports = [
    ./work-common.nix
    # ./wm/xsession-common.nix
    # ./wm/xmonad
    ./wm/hyprland
    # ./wm/niri
  ];
  # gtk.enable = false;
  # qt.enable = false;
  services = {
    # network-manager-applet.enable = false;
    # udiskie.enable = false;
    kbdd.enable = isX11;
    shikane.enable = isHypr;
    picom.enable = isX11;
    screen-locker = {
      enable = lib.mkDefault isX11;
      inactiveInterval = 5;
    };
    hypridle = {
      enable = lib.mkDefault isHypr;
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
            timeout = 30;
            on-timeout = "pidof hyprlock && hyprctl dispatch dpms off";
            on-resume = "hyprctl dispatch dpms on";
          }
        ];
      };
    };

  };
  programs = {
    hyprlock = {
      enable = lib.mkDefault isHypr;
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
      # "eDP-1,disable"
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

{
  pkgs,
  config,
  lib,
  ...
}:
let
  isX11 = config.xsession.enable;
  isHypr = config.wayland.windowManager.hyprland.enable;
  isNiri = config.wrappers.niri.enable;
  isWayland = isHypr || isNiri;
  ideaOptions = pkgs.writeText "idea64.vmoptions" ''
    -Xmx3971m
    -Dawt.toolkit.name=WLToolkit
  '';
in
{
  imports = [
    # ./wm/xsession-common.nix
    # ./wm/xmonad
    # ./wm/hyprland
    ../wm/niri
  ];
  # gtk.enable = false;
  # qt.enable = false;
  services = {
    # network-manager-applet.enable = false;
    # udiskie.enable = false;
    shikane.enable = isWayland;
    picom.enable = isX11;
    screen-locker = {
      enable = lib.mkDefault isX11;
      inactiveInterval = 5;
    };
    hypridle = {
      enable = lib.mkDefault isWayland;
      settings =
        let
          niri = lib.getExe config.wrappers.niri.wrapper;
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
              timeout = 30;
              on-timeout = "pidof hyprlock && ${dpmsOffCmd}";
              on-resume = dpmsOnCmd;
            }
          ];
        };
    };

  };
  programs = {
    hyprlock = {
      enable = lib.mkDefault isWayland;
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
  wrappers = {
    niri.settings = {
      environment = {
        IDEA_VM_OPTIONS = ideaOptions.outPath;
      };
      input.touchpad.tap = _: { };
      workspaces = {
        "browser" = {
          open-on-output = "Dell Inc. DELL P2723D GRJX0V3";
        };
        "dev" = {
          open-on-output = "Dell Inc. DELL P2723D 3MHX0V3";
        };
      };
    };
  };
  wayland.windowManager = {
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
    sway.config = {
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
}

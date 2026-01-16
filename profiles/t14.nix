{
  pkgs,
  myPkgs,
  ...
}:
{
  imports = [
    ./common.nix
    ./cli/atuin
    # ./wm/xsession-common.nix
    # ./wm/xmonad
    # ./wm/hyprland
    ./wm/niri
    ./cli/jujutsu.nix
    ./gui/lutris.nix
  ];
  # imports = [ ./common.nix ];
  # gtk.enable = false;
  # qt.enable = false;
  # services = {
  #   network-manager-applet.enable = false;
  #   udiskie.enable = false;
  # };
  stylix.enable = true;
  xresources.properties =
    let
      dpi = 120;
    in
    {
      "Xft.dpi" = dpi;
      "rofi.dpi" = dpi;
    };
  services = {
    polybar.config = {
      "bar/top" = {
        modules-right = "battery disk memory cpu temperature volume keyboard date tray";

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
    screen-locker = {
      # enable = true;
      inactiveInterval = 60;
    };
  };
  home.packages =
    (with pkgs; [
      brave # needed for messenger calls
      brightnessctl
      borgbackup
      compsize
      exercism
      nautilus
      gtypist
      libreoffice-still
      mpv
      playerctl
      remmina
      tor-browser
      keepassxc
      xpra
      protonup-ng
      steamtinkerlaunch
    ])
    ++ (with myPkgs; [ toggle-touchpad ]);
  home.sessionVariables = {
    BROWSER = "firefox";
  };
  gui = {
    thermal-zone = 0;
    hasBattery = true;
  };
  programs = {
    firefox = {
      enable = true;
    };
    niri.settings.workspaces = {
      "01-browser" = {
        name = "browser";
      };
      "02-game" = {
        name = "game";
      };
    };
  };
  xsession.initExtra = # bash
    ''
      xset s off -dpms
      xrandr --output eDP --set TearFree on
    '';
  wayland.windowManager.hyprland.settings = {
    animations.enabled = false;
    monitor = [ "eDP-1,1920x1080,0x0,1.25" ];
    decoration = {
      blur.enabled = false;
      shadow.enabled = false;
    };
  };
  wayland.windowManager.sway = {
    config = {
      input = {
        "1133:49948:Logitech_USB_Keyboard" = {
          xkb_layout = "lt,us";
          xkb_numlock = "enabled";
        };
        "type:keyboard" = {
          xkb_layout = "lt,us";
          xkb_numlock = "enabled";
        };
      };
      output = {
        "eDP-1" = {
          mode = "1920x1080";
          pos = "0 0";
          adaptive_sync = "on";
          scale = "1.25";
        };
      };
    };
  };
}

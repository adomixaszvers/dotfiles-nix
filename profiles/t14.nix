{ pkgs, myPkgs, ... }: {
  # imports = [ ./common.nix ./wm/xsession-common.nix ./wm/xmonad ];
  imports = [ ./common.nix ];
  gtk.enable = false;
  qt.enable = false;
  services = {
    network-manager-applet.enable = false;
    udiskie.enable = false;
  };
  colors = import ./gui/colors/nord.nix;
  xresources.properties = let dpi = 120;
  in {
    "Xft.dpi" = dpi;
    "rofi.dpi" = dpi;
  };
  services.polybar.config = {
    "bar/top" = {
      modules-right =
        "battery divider memory divider disk divider cpu divider temperature divider volume divider keyboard divider date divider time divider trayer";

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
  home.packages = (with pkgs; [
    brave # needed for messenger calls
    brightnessctl
    borgbackup
    compsize
    discord
    exercism
    gnome.nautilus
    gtypist
    libreoffice-still
    lutris
    mpv
    playerctl
    remmina
    qbittorrent
    tor-browser-bundle-bin
    keepassxc
    xpra
    protonup
  ]) ++ (with myPkgs; [ toggle-touchpad ]);
  home.sessionVariables = { BROWSER = "firefox"; };
  gui.thermal-zone = 1;
  programs = {
    # kitty.settings.shell = "nu";
    nushell.enable = true;
    firefox = {
      enable = true;
      package =
        pkgs.firefox.override { cfg.enablePlasmaBrowserIntegration = true; };
    };
  };
  services.screen-locker = {
    # enable = true;
    inactiveInterval = 60;
  };
  xsession.initExtra = ''
    xset s off -dpms
    xrandr --output eDP --set TearFree on
  '';
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

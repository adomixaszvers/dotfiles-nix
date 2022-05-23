{ pkgs, unstable, inputs, myPkgs, ... }:
let dpi = 120;
in {
  imports = [ ./common.nix ./wm/xsession-common.nix ./wm/xmonad ];
  colors = import ./gui/colors/nord.nix;
  programs.rofi.extraConfig = { inherit dpi; };
  xresources.properties."Xft.dpi" = dpi;
  services.polybar.config = {
    "bar/top-extra" = { inherit dpi; };
    "bar/top" = {
      inherit dpi;
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
  home.packages = (with pkgs; [
    bitwarden
    borgbackup
    compsize
    unstable.discord
    exercism
    gnome.nautilus
    firefox
    gtypist
    mpv
    playerctl
    inputs.nixos-2009.legacyPackages."${system}".remmina
    qbittorrent
    spotify
    torbrowser
    keepassxc
    xpraGtk3
    protonup
  ]) ++ (with myPkgs; [ ani-cli ]);
  home.sessionVariables = { BROWSER = "firefox"; };
  programs.xmobar.thermal-zone = 1;
  services.screen-locker = {
    # enable = true;
    inactiveInterval = 60;
  };
  wayland.windowManager.sway = {
    config.input = {
      "1133:49948:Logitech_USB_Keyboard" = {
        xkb_layout = "lt,us";
        xkb_numlock = "enabled";
      };
    };
  };
}
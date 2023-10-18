{ pkgs, config, lib, ... }: {
  imports = [ ./work-common.nix ./wm/xsession-common.nix ./wm/xmonad ];
  colors = import ./gui/colors/nord.nix;
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

  };
  xsession.windowManager.bspwm = {
    monitors = {
      "DP-1" = [ "1" "2" "3" "4" "5" ];
      "DP-2" = [ "6" "7" "8" "9" "10" ];
    };
  };
  wayland.windowManager.sway.config = {
    startup = [{
      command =
        "${pkgs.swayidle}/bin/swayidle -w timeout 300 '${pkgs.swaylock-effects}/bin/swaylock --clock --screenshots --effect-blur 7x5 --effect-vignette 0.5:0.5'";
    }];
    output = {
      "Hewlett Packard HP E242 CNC614066M" = { pos = "0 0"; };
      "Hewlett Packard HP E242 CNC6430827" = { pos = "1920 0"; };
    };
  };
}

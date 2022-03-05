{ pkgs, ... }: {
  imports =
    [ ./work-common.nix ./common.nix ./wm/xsession-common.nix ./wm/xmonad ];
  colors = import ./gui/colors/nord.nix;
  services.screen-locker = {
    enable = true;
    inactiveInterval = 5;
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
      DP-1 = { pos = "0 0"; };
      DP-2 = { pos = "1920 0"; };
    };
    input = {
      "6127:24729:Lenovo_Lenovo_Traditional_USB_Keyboard" = {
        xkb_layout = "lt,us";
        xkb_numlock = "enabled";
      };
    };
  };
}

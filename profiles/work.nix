{ pkgs, config, lib, ... }:
let dpi = 96;
in {
  imports = [ ./work-common.nix ];
  colors = import ./gui/colors/nord.nix;
  services.screen-locker = {
    enable = lib.mkDefault config.xsession.enable;
    inactiveInterval = 5;
  };
  xsession.windowManager.bspwm = {
    monitors = {
      "DP-1" = [ "1" "2" "3" "4" "5" ];
      "DP-2" = [ "6" "7" "8" "9" "10" ];
    };
    startupPrograms = [ "rambox" ];
  };
  xdg.mimeApps = {
    defaultApplications = {
      "x-scheme-handler/https" = [ "google-chrome.desktop" "firefox.desktop" ];
      "x-scheme-handler/http" = [ "google-chrome.desktop" "firefox.desktop" ];
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
      "Chimei Innolux Corporation 0x15E7 0x00000000" = {
        pos = "3840 0";
        scale = "1.25";
      };
    };
  };
  programs.rofi.extraConfig = { inherit dpi; };
  xresources.properties."Xft.dpi" = dpi;
  services.polybar.config = {
    "bar/top-extra" = { inherit dpi; };
    "bar/top" = { inherit dpi; };
  };
}

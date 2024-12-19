{ pkgs, ... }:
{
  imports = [ ./common.nix ];
  home.packages = with pkgs; [
    firefox
    # gamescope
    mpv
    playerctl
    remmina
    keepassxc
    xpra
  ];
  home.sessionVariables = {
    BROWSER = "firefox";
  };
  stylix = {
    enable = true;
    targets.gtk.enable = false;
  };
  qt.enable = false;
  gui.thermal-zone = 2;
  services = {
    network-manager-applet.enable = false;
    udiskie.enable = false;
  };
}

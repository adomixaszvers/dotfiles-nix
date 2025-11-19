{ pkgs, ... }:
{
  imports = [
    ./common.nix
    ./cli/atuin
    ./cli/jujutsu.nix
  ];
  home.packages = with pkgs; [
    brave
    discord
    # gamescope
    libreoffice-still
    lutris
    mpv
    (obs-studio.override {
      cudaSupport = true;
    })
    playerctl
    remmina
    keepassxc
    xpra
    protonup-qt
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
  xsession.initExtra = # bash
    ''
      xset s off -dpms
      xrandr --output eDP --set TearFree on
    '';
  wayland.windowManager.hyprland.settings.monitor = [
    "DP-3,1920x1080@144,0x0,1"
    "DP-2,1920x1080@60,1920x0,1"
  ];
}

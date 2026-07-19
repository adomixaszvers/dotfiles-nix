{ pkgs, ... }:
{
  imports = [
    ./common.nix
    ./cli/atuin
    ./cli/jujutsu.nix
    ./gui/lutris.nix
  ];
  home.packages = with pkgs; [
    # keep-sorted start block=yes
    (obs-studio.override {
      cudaSupport = true;
    })
    brave
    keepassxc
    # gamescope
    libreoffice-still
    mpv
    playerctl
    protonup-qt
    remmina
    xpra
    # keep-sorted end
  ];
  home.sessionVariables = {
    BROWSER = "firefox";
  };

  programs.emacs.package = pkgs.emacs-pgtk;

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
    {
      output = "DP-3";
      mode = "1920x1080@144";
      position = "0x0";
      scale = 1;
    }
    {
      output = "DP-2";
      mode = "1920x1080@60";
      position = "1920x0";
      scale = 1;
    }
  ];
}

{ pkgs, ... }:
{
  imports = [
    ./common.nix
    ./cli/atuin
    ./cli/jujutsu.nix
    ./wm/xsession-common.nix
    ./wm/xmonad
  ];
  home.packages = with pkgs; [
    # keep-sorted start
    borgbackup
    brave
    compsize
    exercism
    gtypist
    keepassxc
    mpv
    nautilus
    playerctl
    protonup-ng
    qbittorrent
    remmina
    tor-browser
    xpra
    # keep-sorted end
  ];
  home.sessionVariables = {
    BROWSER = "firefox";
  };
  gui.thermal-zone = 1;
  services = {
    network-manager-applet.enable = false;
    polybar.config."module/disk" = {
      mount-1 = "/home";
    };
    screen-locker = {
      # enable = true;
      inactiveInterval = 60;
    };
  };
  stylix.enable = true;
  wayland.windowManager.sway = {
    config.input = {
      "1133:49948:Logitech_USB_Keyboard" = {
        xkb_layout = "lt,us";
        xkb_numlock = "enabled";
      };
    };
  };
}

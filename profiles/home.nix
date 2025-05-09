{ pkgs, ... }:
{
  imports = [
    ./common.nix
    ./wm/xsession-common.nix
    ./wm/xmonad
  ];
  home.packages = with pkgs; [
    brave
    borgbackup
    compsize
    discord
    exercism
    nautilus
    gtypist
    mpv
    playerctl
    remmina
    qbittorrent
    spotify
    tor-browser-bundle-bin
    keepassxc
    xpra
    protonup
  ];
  home.sessionVariables = {
    BROWSER = "firefox";
  };
  programs.autorandr = {
    profiles = {
      home-prime = {
        fingerprint = {
          LVDS-1 = "00ffffffffffff0006afec260000000001130103802213780ad7759355558d29245054000000010101010101010101010101010101010c1c56a0500010303020360058c1100000180000000f0000000000000000000000000020000000fe0041554f0a202020202020202020000000fe004231353658573032205636200a0065";
          HDMI-1 = "00ffffffffffff004c2d440e50475230131b010381351e782a1c05b04e47a826105054bfef8081c0810081809500a9c0b300714f01010474801871382d40582c450014302100001e000000fd0032781e881e000a202020202020000000fc00433234464737780a2020202020000000ff004854484a3530313835300a2020015102031df14890041f130312403f230907078301000067030c002000803c047480d072382d40102c458014302100001e023a801871382d40582c450014302100001e011d007251d01e206e28550014302100001e8c0ad08a20e02d10103e96001430210000180000000000000000000000000000000000000000000000000000b7";
        };
        config = {
          DP-1.enable = false;
          VGA-1.enable = false;
          LVDS-1 = {
            enable = true;
            position = "1920x312";
            rate = "60.00";
            mode = "1366x768";
          };
          HDMI-1 = {
            enable = true;
            position = "0x0";
            primary = true;
            rate = "60.00";
            mode = "1920x1080";
          };
        };
        hooks.postswitch = "systemctl --user restart picom.service";
      };
    };
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

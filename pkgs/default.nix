{ pkgs, system, inputs }:

{
  ani-cli = pkgs.callPackage ./ani-cli { ani-cli = inputs.ani-cli.outPath; };
  bspwm-greedy-focus = pkgs.callPackage ./bspwm-greedy-focus.nix { };
  bspwm-reorder-desktops = pkgs.callPackage ./bspwm-reorder-desktops.nix { };
  bumblebee-status = pkgs.callPackage ./bumblebee-status {
    bumblebee-status-source = inputs.bumblebee-status.outPath;
  };
  dbvisualizer = pkgs.callPackage ./dbvisualizer.nix { };
  hm-switch = pkgs.callPackage ./hm-switch.nix {
    inherit (inputs.home-manager.packages."${system}") home-manager;
  };
  kaknix = pkgs.callPackage ./kaknix.nix { };
  inherit (import ./lua-fmt { inherit pkgs; }) lua-fmt;
  maimpick = pkgs.callPackage ./maimpick.nix { };
  otpauth = pkgs.callPackage ./otpauth { };
  rofi-powermenu = pkgs.callPackage ./rofi-powermenu.nix { };
  sxhkd = pkgs.sxhkd.overrideAttrs (_: { patches = [ ./sxhkd.patch ]; });
  steam = pkgs.steam.override {
    extraPkgs = ps:
      with ps; [
        atk
        cairo
        dbus
        fontconfig
        freetype
        gdk_pixbuf
        glib
        gnome3.gtk
        lsb-release
        pango
        xorg.libxcb
        zlib
      ];
  };
  vimgolf = pkgs.callPackage ./vimgolf { };
}

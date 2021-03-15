{ pkgs, bumblebee-status-source }:

{
  bspwm-greedy-focus = pkgs.callPackage ./bspwm-greedy-focus.nix { };
  bspwm-reorder-desktops =
    pkgs.callPackage ./bspwm-reorder-desktops.nix { };
  bumblebee-status = pkgs.callPackage ./bumblebee-status {
    inherit bumblebee-status-source;
  };
  dbvisualizer = pkgs.callPackage ./dbvisualizer.nix { };
  kaknix = pkgs.callPackage ./kaknix.nix { };
  lua-fmt = (import ./lua-fmt { inherit pkgs; }).lua-fmt;
  maimpick = pkgs.callPackage ./maimpick.nix { };
  rofi-powermenu = pkgs.callPackage ./rofi-powermenu.nix { };
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

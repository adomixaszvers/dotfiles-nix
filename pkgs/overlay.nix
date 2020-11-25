pkgs: super: {
  sxhkd = super.sxhkd.overrideAttrs (_: { patches = [ ./sxhkd.patch ]; });
  mine = {
    bspwm-greedy-focus = super.callPackage ./bspwm-greedy-focus.nix { };
    bspwm-reorder-desktops =
      super.callPackage ./bspwm-reorder-desktops.nix { };
    bumblebee-status = super.callPackage ./bumblebee-status { };
    dbvisualizer = super.callPackage ./dbvisualizer.nix { };
    kaknix = super.callPackage ./kaknix.nix { };
    lua-fmt = (import ./lua-fmt { inherit pkgs; }).lua-fmt;
    maimpick = super.callPackage ./maimpick.nix { };
    rofi-powermenu = super.callPackage ./rofi-powermenu.nix { };
    steam = super.steam.override {
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
    vimgolf = super.callPackage ./vimgolf { };
  };
}

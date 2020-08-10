pkgs: super: {
  sxhkd = super.sxhkd.overrideAttrs (_: { patches = [ ./pkgs/sxhkd.patch ]; });
  mine = {
    bspwm-greedy-focus = super.callPackage ./pkgs/bspwm-greedy-focus.nix { };
    bspwm-reorder-desktops =
      super.callPackage ./pkgs/bspwm-reorder-desktops.nix { };
    bumblebee-status = super.callPackage ./pkgs/bumblebee-status { };
    dbvisualizer = super.callPackage ./pkgs/dbvisualizer.nix { };
    kaknix = super.callPackage ./pkgs/kaknix.nix { };
    lua-fmt = (import ./pkgs/lua-fmt { inherit pkgs; }).lua-fmt;
    maimpick = super.callPackage ./pkgs/maimpick.nix { };
    rofi-powermenu = super.callPackage ./pkgs/rofi-powermenu.nix { };
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
    vimgolf = super.callPackage ./pkgs/vimgolf { };
  };
}
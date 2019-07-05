{ pkgs ? import <nixpkgs> { } }: {
  bumblebee-status = pkgs.callPackage ./bumblebee-status { };
  steam = pkgs.steam.override {
    withPrimus = true;
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

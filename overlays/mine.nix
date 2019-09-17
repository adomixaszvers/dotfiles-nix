self: super: {
  mine = {
    bumblebee-status = super.callPackage ../pkgs/bumblebee-status { };
    vimgolf = super.callPackage ../pkgs/vimgolf { };
    maimpick = super.callPackage ../pkgs/maimpick.nix { };
    rofi-powermenu = super.callPackage ../pkgs/rofi-powermenu.nix { };
    sxhkd = super.sxhkd.overrideAttrs
      (oldAttrs: { patches = [ ../pkgs/sxhkd.patch ]; });
    ghc = let
      haskellPackages = ps:
        with ps; [
          # ghc-mod
          hasktags
          # hdevtools
          hindent
          hlint
          # hoogle
          hspec
          pointfree
          pointful
          stylish-haskell
        ];
    in self.ghc.withHoogle haskellPackages;
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
  };
}

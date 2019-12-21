self: super: {
  kakounePlugins = super.kakounePlugins // {
    kakoune-text-objects =
      super.callPackage ../pkgs/kakoune-text-objects.nix { };
  };
  sxhkd = super.sxhkd.overrideAttrs
    (oldAttrs: { patches = [ ../pkgs/sxhkd.patch ]; });
  mine = {
    bumblebee-status = super.callPackage ../pkgs/bumblebee-status { };
    lcf = super.callPackage ../pkgs/lcf.nix { luaPackages = self.lua53Packages; };
    vimgolf = super.callPackage ../pkgs/vimgolf { };
    maimpick = super.callPackage ../pkgs/maimpick.nix { };
    rofi-powermenu = super.callPackage ../pkgs/rofi-powermenu.nix { };
    kaknix = super.callPackage ../pkgs/kaknix.nix { };
    dbxcli = super.callPackage ../pkgs/dbxcli { };
    dbvisualizer = super.callPackage ../pkgs/dbvisualizer.nix { };
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

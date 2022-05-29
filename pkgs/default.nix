{ pkgs, unstable, system, inputs }:

{
  ani-cli = pkgs.callPackage ./ani-cli { ani-cli = inputs.ani-cli.outPath; };
  bspwm-greedy-focus = pkgs.callPackage ./bspwm-greedy-focus.nix { };
  bspwm-reorder-desktops = pkgs.callPackage ./bspwm-reorder-desktops.nix { };
  bumblebee-status = pkgs.callPackage ./bumblebee-status {
    bumblebee-status-source = inputs.bumblebee-status.outPath;
  };
  custom-xrdp = pkgs.callPackage ./custom-xrdp { };
  dbvisualizer = pkgs.callPackage ./dbvisualizer.nix { };
  hunspell-lt = pkgs.callPackage ./hunspell-lt { };
  hm-option = pkgs.callPackage ./hm-option.nix { };
  hm-repl = pkgs.callPackage ./hm-repl.nix { };
  hm-switch = pkgs.callPackage ./hm-switch.nix {
    inherit (inputs.home-manager.packages."${system}") home-manager;
  };
  kaknix = pkgs.callPackage ./kaknix.nix { };
  maimpick = pkgs.callPackage ./maimpick.nix { };
  neovim = pkgs.callPackage ../profiles/cli/neovim/package.nix { };
  otpauth = pkgs.callPackage ./otpauth { };
  rivercarro = let inherit (unstable) zig;
  in pkgs.callPackage ./rivercarro { inherit zig; };
  rofi-powermenu = pkgs.callPackage ./rofi-powermenu.nix { };
  sxhkd = pkgs.sxhkd.overrideAttrs (_: { patches = [ ./sxhkd.patch ]; });
  sway-greedy-focus = pkgs.callPackage ./sway-greedy-focus.nix { };
  vimgolf = pkgs.callPackage ./vimgolf { };
} // (pkgs.lib.attrsets.optionalAttrs (system == "x86_64-linux") {
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
        gnome.gtk
        lsb-release
        pango
        xorg.libxcb
        zlib
      ];
  };
  idea-ultimate = pkgs.jetbrains.idea-ultimate.overrideAttrs (_old: rec {
    name = "idea-ultimate-${version}";
    version = "2022.1.1";
    src = pkgs.fetchurl {
      url =
        "https://download.jetbrains.com/idea/ideaIU-${version}-no-jbr.tar.gz";
      sha256 = "0g31cjkyzzi6s8whv4a55fpjviiz02jwlqap2620s5mpi5a636f1";
    };
  });
})

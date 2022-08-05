{ pkgs, system, inputs }:

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
  rivercarro = pkgs.callPackage ./rivercarro { };
  rofi-powermenu = pkgs.callPackage ./rofi-powermenu.nix { };
  sxhkd = pkgs.sxhkd.overrideAttrs (_: { patches = [ ./sxhkd.patch ]; });
  sway-greedy-focus = pkgs.callPackage ./sway-greedy-focus.nix { };
  toggle-touchpad = pkgs.callPackage ./toggle-touchpad.nix { };
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
        gdk-pixbuf
        glib
        gtk3
        lsb-release
        pango
        xorg.libxcb
        zlib
      ];
  };
  idea-ultimate =
    let jdk = pkgs.callPackage ./idea-ultimate/jetbrains-jdk.nix { };
    in (pkgs.jetbrains.idea-ultimate.override { inherit jdk; }).overrideAttrs
    (_old: rec {
      name = "idea-ultimate-${version}";
      version = "2022.2";
      src = pkgs.fetchurl {
        url =
          "https://download.jetbrains.com/idea/ideaIU-${version}-no-jbr.tar.gz";
        sha256 = "1qvgac89psc9szri0c4c8aky4nf7f138h4flb9hzn8zpqwkx1v6w";
      };
    });
})

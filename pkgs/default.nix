{ pkgs, system, inputs }: {
  bspwm-greedy-focus = pkgs.callPackage ./bspwm-greedy-focus.nix { };
  bspwm-reorder-desktops = pkgs.callPackage ./bspwm-reorder-desktops.nix { };
  bumblebee-status = pkgs.callPackage ./bumblebee-status {
    bumblebee-status-source = inputs.bumblebee-status.outPath;
  };
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
  restart-eww = pkgs.callPackage ./restart-eww.nix { };
  rivercarro = pkgs.callPackage ./rivercarro { };
  rofi-powermenu = pkgs.callPackage ./rofi-powermenu.nix { };
  sxhkd = pkgs.sxhkd.overrideAttrs (_: { patches = [ ./sxhkd.patch ]; });
  sway-greedy-focus = pkgs.callPackage ./sway-greedy-focus.nix { };
  toggle-touchpad = pkgs.callPackage ./toggle-touchpad.nix { };
  keystore-explorer = pkgs.keystore-explorer.override { jdk = pkgs.openjdk8; };
  tail-volume = pkgs.callPackage ./tail-volume { };
}

{
  perSystem = { pkgs, inputs', ... }: {
    packages = {
      bspwm-greedy-focus = pkgs.callPackage ./bspwm-greedy-focus.nix { };
      bspwm-reorder-desktops =
        pkgs.callPackage ./bspwm-reorder-desktops.nix { };
      bumblebee-status = pkgs.callPackage ./bumblebee-status { };
      hunspell-lt = pkgs.callPackage ./hunspell-lt { };
      he = pkgs.callPackage ./he.nix { };
      hypr-window-select = pkgs.callPackage ./hypr-window-select.nix { };
      hypr-greedy-focus = pkgs.callPackage ./hypr-greedy-focus.nix { };
      hm-option = pkgs.callPackage ./hm-option.nix { };
      hm-repl = pkgs.callPackage ./hm-repl.nix { };
      hm-switch = pkgs.callPackage ./hm-switch.nix {
        inherit (inputs'.home-manager.packages) home-manager;
      };
      kaknix = pkgs.callPackage ./kaknix.nix { };
      maimpick = pkgs.callPackage ./maimpick.nix { };
      neovim = pkgs.callPackage ../profiles/cli/neovim/package.nix { };
      nixvim = inputs'.nixvim.legacyPackages.makeNixvimWithModule {
        inherit pkgs;
        module = { imports = [ ./nixvim/base.nix ]; };
      };
      nixvimLsp = inputs'.nixvim.legacyPackages.makeNixvimWithModule {
        inherit pkgs;
        module = { imports = [ ./nixvim/base.nix ./nixvim/lsp.nix ]; };
      };
      restart-eww = pkgs.callPackage ./restart-eww.nix { };
      rivercarro = pkgs.callPackage ./rivercarro { };
      rofi-powermenu = pkgs.callPackage ./rofi-powermenu.nix { };
      sxhkd = pkgs.sxhkd.overrideAttrs (_: { patches = [ ./sxhkd.patch ]; });
      sway-greedy-focus = pkgs.callPackage ./sway-greedy-focus.nix { };
      toggle-touchpad = pkgs.callPackage ./toggle-touchpad.nix { };
      keystore-explorer =
        pkgs.keystore-explorer.override { jdk = pkgs.openjdk8; };
      tail-volume = pkgs.callPackage ./tail-volume { };
    };
  };
}

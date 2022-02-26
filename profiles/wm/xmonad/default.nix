{ pkgs, system, inputs, config, ... }:
let
  extraPackages = import ./extraPackages.nix;
  inherit (builtins.getAttr system inputs.nixpkgs-hs.legacyPackages)
    haskellPackages;
in {
  imports = [ ../polybar.nix ../dunst.nix ../picom.nix ];
  home.packages = with pkgs; [ pamixer xdotool gnome.zenity ];
  services.volnoti.enable = true;
  services.polybar.config = {
    "bar/top".modules-left = "xmonad";
    "bar/top-extra".modules-left = "xmonad";
    "module/xmonad" = {
      type = "custom/script";
      exec = "${haskellPackages.xmonad-dbus}/bin/xmonad-dbus $SCREEN_ID";
      tail = true;
    };
  };
  xsession.windowManager.xmonad = {
    inherit extraPackages;
    enable = true;
    config = ./xmonad.hs;
    inherit haskellPackages;
    libFiles = {
      "Colors.hs" = pkgs.writeText "Colors.hs" ''
        module Colors where

        ${builtins.concatStringsSep "\n" (builtins.attrValues (builtins.mapAttrs
          (name: value: ''
            ${name} :: String
            ${name} = "${value}"
          '') config.colors))}'';
    };
  };
}

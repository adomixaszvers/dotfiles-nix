{ pkgs, unstable, config, ... }:
let
  extraPackages = import ./extraPackages.nix;
  haskellPackages = unstable.haskellPackages.override {
    overrides = _: super: {
      xmonad = super.xmonad_0_17_0;
      xmonad-contrib = super.xmonad-contrib_0_17_0;
      xmonad-dbus = unstable.haskell.lib.dontCheck
        (unstable.haskell.lib.unmarkBroken super.xmonad-dbus);
    };
  };
in {
  imports = [ ../polybar.nix ../dunst.nix ../picom.nix ];
  home.packages = with pkgs; [ pamixer xdotool gnome3.zenity ];
  services.volnoti.enable = true;
  services.polybar.config = {
    "bar/top".modules-left = "xmonad";
    "bar/top-extra".modules-left = "xmonad";
    "module/xmonad" = {
      type = "custom/script";
      exec =
        "${haskellPackages.xmonad-dbus}/bin/xmonad-dbus /org/XMonad/$SCREEN_ID";
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

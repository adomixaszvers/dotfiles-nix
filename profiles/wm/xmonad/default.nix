{ pkgs, unstable, inputs, config, ... }:
let extraPackages = import ./extraPackages.nix;
in {
  imports = [ ../polybar.nix ../dunst.nix ../picom.nix ];
  home.packages = with pkgs; [ pamixer xdotool gnome3.zenity ];
  services.volnoti.enable = true;
  services.polybar.config = {
    "bar/top".modules-left = "xmonad";
    "bar/top-extra".modules-left = "xmonad";
    "module/xmonad" =
      let inherit (inputs.xmonad-dbus.packages.x86_64-linux) xmonad-dbus;
      in {
        type = "custom/script";
        exec = "${xmonad-dbus}/bin/xmonad-dbus -p /org/xmonad/Log/$SCREEN_ID";
        tail = true;
      };
  };
  xsession.windowManager.xmonad = {
    inherit extraPackages;
    enable = true;
    config = ./xmonad.hs;
    haskellPackages = unstable.haskellPackages.override {
      overrides = _: super: {
        xmonad = super.xmonad_0_17_0;
        xmonad-contrib = super.xmonad-contrib_0_17_0;
      };
    };
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

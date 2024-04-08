{ config, pkgs, lib, ... }:
let
  extraPackages = import ./extraPackages.nix;
  inherit (pkgs) haskellPackages;
  inherit (haskellPackages) xmonad-dbus;
  launch-polybar = pkgs.writeShellScriptBin "launch-polybar" ''
    PATH=$PATH:${with pkgs; lib.makeBinPath [ coreutils gnugrep xorg.xrandr ]}

    SCREEN_ID="$1"
    MONITOR="$(xrandr --listactivemonitors | grep "''${SCREEN_ID}:" | cut -d' ' -f6)"
    if [ "$SCREEN_ID" = 0 ]; then
      BAR=top
    else
      BAR=top-extra
    fi
    SCREEN_ID="$SCREEN_ID" MONITOR="$MONITOR" polybar "$BAR"
  '';
in {
  imports = [ ../dunst.nix ../picom.nix ../polybar.nix ];
  home.packages = (with pkgs; [ pamixer xdotool gnome.zenity ])
    ++ [ xmonad-dbus launch-polybar ];
  programs.polybar.enable = true;
  services.polybar.enable = false;
  services.polybar.config = {
    "bar/top".modules-left = "xmonad";
    "bar/top-extra".modules-left = "xmonad";
    "module/xmonad" = {
      type = "custom/script";
      exec = "${xmonad-dbus}/bin/xmonad-dbus $SCREEN_ID";
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

        white, cyan :: String
        white = "#${config.lib.stylix.colors.base00}"
        cyan = "#${config.lib.stylix.colors.cyan}"
      '';
    };
  };
}

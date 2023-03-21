{ config, pkgs, lib, myPkgs, inputs, system, ... }:
let
  extraPackages = import ./extraPackages.nix;
  inherit (pkgs) haskellPackages;
  inherit (haskellPackages) xmonad-dbus;
  inherit (inputs.eww.packages.${system}) eww;
  trayer-toggle = pkgs.writeShellScriptBin "toggle-trayer" ''
    PATH=$PATH:${lib.makeBinPath [ pkgs.xdo eww ]}

    filename="$XDG_RUNTIME_DIR/trayer-hidden$DISPLAY"

    if [ ! -f "$filename" ]; then
      touch "$filename"
      xdo hide -N trayer
      eww update trayer-visible=false
    else
      rm "$filename"
      xdo show -N trayer 
      xdo raise -N trayer 
      eww update trayer-visible=true
    fi
  '';
in {
  imports = [ ../dunst.nix ../picom.nix ];
  home.packages = (with pkgs; [ pamixer xdotool gnome.zenity ])
    ++ [ eww myPkgs.tail-volume trayer-toggle xmonad-dbus ];
  xdg.configFile."eww/eww.yuck".source = ./eww/eww.yuck;
  xdg.configFile."eww/eww.scss".source = ./eww/eww.scss;
  services.polybar.config = {
    "bar/top".modules-left = "xmonad";
    "bar/top-extra".modules-left = "xmonad";
    "module/xmonad" = {
      type = "custom/script";
      exec = "${xmonad-dbus}/bin/xmonad-dbus $SCREEN_ID";
      tail = true;
    };
  };
  services.trayer = {
    enable = true;
    settings = {
      monitor = "primary";
      distance = 20;
      tint = builtins.replaceStrings [ "#" ] [ "0x" ] config.colors.background;
      alpha = 0;
      edge = "top";
      align = "right";
      expand = false;
      transparent = true;
      widthtype = "request";
      height = 16;
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

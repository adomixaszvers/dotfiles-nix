{ pkgs, config, ... }:
let
  extraPackages = import ./extraPackages.nix;
  xmonadFifo = pkgs.writeShellScriptBin "xmonadFifo.sh" ''
    set -e
    FIFO_FILE=/run/user/$UID/xmonad-fifo-$1
    [ -p $FIFO_FILE ] || mkfifo -m 600 "$FIFO_FILE"
    # Open file descriptor (fd) 3 for read/write on a text file.
    exec 3<> "$FIFO_FILE"
    tee "$FIFO_FILE" 1>/dev/null
    # Close fd 3
    exec 3>&-
  '';
in {
  imports = [ ../polybar.nix ../dunst.nix ../picom.nix ];
  home.packages = with pkgs; [ pamixer xdotool gnome3.zenity xmonadFifo ];
  services.volnoti.enable = true;
  services.polybar.config = {
    "bar/top".modules-left = "xmonad";
    "bar/top-extra".modules-left = "xmonad";
    "module/xmonad" = {
      type = "custom/script";
      exec = "cat /run/user/$UID/xmonad-fifo-$SCREEN_ID";
      tail = true;
    };
  };
  xsession.windowManager.xmonad = {
    inherit extraPackages;
    enable = true;
    config = ./xmonad.hs;
    haskellPackages = pkgs.nixos-unstable.haskellPackages.override {
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

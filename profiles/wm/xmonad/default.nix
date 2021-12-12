{ pkgs, ... }:
let
  extraPackages = import ./extraPackages.nix;
  xmonadFifo = pkgs.writeShellScriptBin "xmonadFifo.sh" ''
    mkfifo /run/user/$UID/xmonad-fifo-$1
    exec tee /run/user/$UID/xmonad-fifo-$1 1>/dev/null
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
  };
}

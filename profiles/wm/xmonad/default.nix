{ pkgs, ... }:
let extraPackages = import ./extraPackages.nix;
in {
  imports = [ ../polybar.nix ../dunst.nix ../picom.nix ];
  home.packages = with pkgs; [ pamixer xdotool gnome3.zenity ];
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

{ pkgs, ... }:
let extraPackages = import ./extraPackages.nix;
in {
  imports = [ ./xmobar.nix ../dunst.nix ../picom.nix ];
  home.packages = with pkgs; [ pamixer xdotool gnome3.zenity ];
  services.volnoti.enable = true;
  xsession.windowManager.xmonad = {
    inherit extraPackages;
    enable = true;
    config = ./xmonad.hs;
  };
}

{ pkgs, ... }:
let extraPackages = import ./extraPackages.nix;
in {
  imports = [ ./xmobar.nix ../dunst.nix ../picom.nix ];
  home.packages = with pkgs; [ gnome3.zenity ];
  xsession.windowManager.xmonad = {
    inherit extraPackages;
    enable = true;
    config = ./xmonad.hs;
  };
}

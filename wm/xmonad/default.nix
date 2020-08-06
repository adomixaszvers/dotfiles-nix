{ pkgs, ... }:
let extraPackages = import ./extraPackages.nix;
in {
  imports = [ ../polybar.nix ../dunst.nix ../picom.nix ];
  home.packages = with pkgs; [ gnome3.zenity ];
  services.polybar.config = {
    "bar/top".modules-left = "xmonad";
    "bar/top-extra".modules-left = "xmonad";
    "module/xmonad" = {
      type = "custom/script";
      exec = "${pkgs.xmonad-log}/bin/xmonad-log";
      tail = true;
    };
  };
  xsession.windowManager.xmonad = {
    inherit extraPackages;
    enable = true;
    config = ./xmonad.hs;
  };
}

{ pkgs, config, ... }:
let extraPackages = import ./extraPackages.nix;
in {
  imports = [ ./xmobar.nix ../dunst.nix ../picom.nix ];
  home.packages = with pkgs; [ gnome3.zenity ];
  services.trayer = {
    enable = true;
    settings = {
      edge = "top";
      SetDockType = true;
      SetPartialStrut = true;
      align = "right";
      expand = true;
      widthtype = "request";
      height = 17;
      alpha = 0;
      transparent = true;
      tint =
        builtins.replaceStrings [ "#" ] [ "0xff" ] config.colors.background;
    };
  };
  xsession.windowManager.xmonad = {
    inherit extraPackages;
    enable = true;
    config = ./xmonad.hs;
  };
}

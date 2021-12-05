{ pkgs, ... }:
let extraPackages = import ./extraPackages.nix;
in {
  imports = [ ./xmobar.nix ../dunst.nix ../picom.nix ];
  home.packages = with pkgs; [ gnome3.zenity ];
  services.trayer = {
    enable = true;
    settings = {
      edge = "top";
      padding = 6;
      SetDockType = true;
      SetPartialStrut = true;
      align = "right";
      expand = true;
      widthtype = "request";
      height = 17;
      heighttype = "request";
      alpha = 0;
      transparent = true;
      tint = "0xFFFFFF";
    };
  };
  xsession.windowManager.xmonad = {
    inherit extraPackages;
    enable = true;
    config = ./xmonad.hs;
  };
}

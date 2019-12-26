{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [ rxvt_unicode-with-plugins ];
  xresources.properties = with config.lib.colors; {
    "URxvt*font" = "xft:SauceCodePro Nerd Font Mono:size=10";
    "URxvt.perl-ext-common" =
      "default,matcher,selection-to-clipboard,font-size";
    "URxvt.url-launcher" = "/usr/bin/xdg-open";
    "URxvt.matcher.button" = "1";
    "URxvt.letterSpace" = "-1";
    "URxvt.iso14755" = "false";
    "URxvt.iso14755_52" = "false";
    "URxvt.scrollBar" = "false";
    "URxvt.background" = background;
    "urxvt*depth" = "32";

    "URxvt.keysym.C-Up" = "font-size:increase";
    "URxvt.keysym.C-Down" = "font-size:decrease";
    "URxvt.keysym.C-S-Up" = "font-size:incglobal";
    "URxvt.keysym.C-S-Down" = "font-size:decglobal";
    "URxvt.keysym.C-equal" = "font-size:reset";
    "URxvt.keysym.C-slash" = "font-size:show";
  };
}

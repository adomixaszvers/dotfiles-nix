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

    # special
    "*.foreground" = foreground;
    "*.background" = background;
    "*.cursorColor" = cursorColor;

    # black
    "*.color0" = black;
    "*.color8" = blackb;

    # red
    "*.color1" = red;
    "*.color9" = redb;

    # green
    "*.color2" = green;
    "*.color10" = greenb;

    # yellow
    "*.color3" = yellow;
    "*.color11" = yellowb;

    # blue
    "*.color4" = blue;
    "*.color12" = blueb;

    # magenta
    "*.color5" = magenta;
    "*.color13" = magentab;

    # cyan
    "*.color6" = cyan;
    "*.color14" = cyanb;

    # white
    "*.color7" = white;
    "*.color15" = whiteb;
  };
}

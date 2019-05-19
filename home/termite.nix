{ pkgs, config, ... }: {
  programs.termite = {
    enable = true;
    allowBold = true;
    browser = "${pkgs.xdg_utils}/xdg-open";
    clickableUrl = true;
    colorsExtra = with config.lib.colors; ''
      # special
      foreground      = ${foreground}
      foreground_bold = ${foreground}
      cursor          = ${cursorColor}
      background      = ${background}

      # black
      color0  = ${black}
      color8  = ${blackb}

      # red
      color1  = ${red}
      color9  = ${redb}

      # green
      color2  = ${green}
      color10 = ${greenb}

      # yellow
      color3  = ${yellow}
      color11 = ${yellowb}

      # blue
      color4  = ${blue}
      color12 = ${blueb}

      # magenta
      color5  = ${magenta}
      color13 = ${magentab}

      # cyan
      color6  = ${cyan}
      color14 = ${cyanb}

      # white
      color7  = ${white}
      color15 = ${whiteb}
    '';
    font = "FuraMono Nerd Font Mono 9";
  };
}

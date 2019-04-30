{ config, lib, ... }:
with (lib.mapAttrs (n: v: builtins.replaceStrings [ "#" ] [ "0x" ] v) config.lib.colors);
{
  programs.alacritty = {
    enable = true;
    settings = {
      colors = {
        primary = { inherit background foreground; };
        cursor = { cursor = cursorColor; };
        normal = {
          inherit black white;
          red = redb;
          green = greenb;
          yellow = yellowb;
          blue = blueb;
          magenta = magentab;
          cyan = cyanb;
        };
        bright = {
          inherit red green yellow blue magenta cyan;
          black = blackb;
          white = whiteb;
        };
      };
      font = let fontSettings = { family = "FuraMono Nerd Font Mono"; }; in {
        normal = fontSettings;
        bold = fontSettings;
        italic = fontSettings;
        size = 9;
      };
    };
  };
}

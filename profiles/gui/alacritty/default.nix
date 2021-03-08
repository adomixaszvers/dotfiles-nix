{ config, lib, ... }:
with (lib.mapAttrs (_: builtins.replaceStrings [ "#" ] [ "0x" ])
  config.colors); {
    programs.alacritty = {
      enable = true;
      settings = {
        background_opacity = 1.0;
        colors = {
          primary = { inherit background foreground; };
          cursor = { cursor = cursorColor; };
          normal = { inherit black white red green yellow blue magenta cyan; };
          bright = {
            black = blackb;
            white = whiteb;
            red = redb;
            green = greenb;
            yellow = yellowb;
            blue = blueb;
            magenta = magentab;
            cyan = cyanb;
          };
        };
        font = let fontSettings = { family = "FiraCode Nerd Font Mono"; };
        in {
          normal = fontSettings;
          bold = fontSettings;
          italic = fontSettings;
          size = 9;
        };
      };
    };
  }

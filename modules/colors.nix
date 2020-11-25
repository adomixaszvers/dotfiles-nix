{ config, lib, ... }:
let
  hexToDec = let
    charMap = {
      "0" = 0;
      "1" = 1;
      "2" = 2;
      "3" = 3;
      "4" = 4;
      "5" = 5;
      "6" = 6;
      "7" = 7;
      "8" = 8;
      "9" = 9;
      "a" = 10;
      "b" = 11;
      "c" = 12;
      "d" = 13;
      "e" = 14;
      "f" = 15;
      "A" = 10;
      "B" = 11;
      "C" = 12;
      "D" = 13;
      "E" = 14;
      "F" = 15;
    };
    charToNumber = x: builtins.getAttr x charMap;
  in hex:
  builtins.foldl' (acc: ele: acc * 16 + charToNumber ele) 0
  (lib.stringToCharacters hex);
  toTrgba = hex:
    let
      f = i: toString (hexToDec (builtins.substring i 2 hex));
      r = f 1;
      g = f 3;
      b = f 5;
    in "rgba(${r}, ${g}, ${b}, 0.95)";
  cfg = config.colors;
in {
  options.colors = with lib;
    let
      hex = types.strMatching "^#[a-f0-9]{6}$";
      mkHexOption = description:
        mkOption {
          inherit description;
          type = hex;
        };
    in mkOption {
      description = "Common colors for applications";
      type = with types;
        submodule {
          options = {
            # transparency = mkOption {
            #   description = "Transparency used in some applications (0-100)";
            #   type = addCheck int (x: x >= 0 && x <= 100);
            # };
            foreground = mkHexOption "Foreground color";
            background = mkHexOption "Background color";
            backgroundTRGBA = mkOption { type = str; };
            backgroundTransparent = mkOption { type = str; };
            cursorColor = mkHexOption "Cursor color";

            black = mkHexOption "Black color";
            blackb = mkHexOption "Black (bright) color";

            red = mkHexOption "Red color";
            redb = mkHexOption "Red (bright) color";

            green = mkHexOption "Green color";
            greenb = mkHexOption "Green (bright) color";

            yellow = mkHexOption "Yellow color";
            yellowb = mkHexOption "Yellow (bright) color";

            blue = mkHexOption "Blue color";
            blueb = mkHexOption "Blue (bright) color";

            magenta = mkHexOption "Magenta color";
            magentab = mkHexOption "Magenta (bright) color";

            cyan = mkHexOption "Cyan color";
            cyanb = mkHexOption "Cyan (bright) color";

            white = mkHexOption "White color";
            whiteb = mkHexOption "White (bright) color";
          };
        };
    };
  config.colors = {
    backgroundTRGBA = lib.mkDefault (toTrgba cfg.background);
    backgroundTransparent = lib.mkDefault "[95]${cfg.background}";
  };
}

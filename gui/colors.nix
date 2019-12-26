{ lib, ... }:
let
  hexToDec = import ./hexToDec.nix { inherit lib; };
  toTrgba = hex:
    let
      f = i: toString (hexToDec (builtins.substring i 2 hex));
      r = f 1;
      g = f 3;
      b = f 5;
    in "rgba(${r}, ${g}, ${b}, 0.95)";
  _4ban = {
    foreground = "#f2efed";
    background = "#282828";
    backgroundTRGBA = toTrgba "#282828";
    backgroundTransparent = "[95]#282828";
    cursorColor = "#c8ccd4";

    black = "#32302f";
    blackb = "#928374";

    red = "#d64d4d";
    redb = "#fb4934";

    green = "#00b159";
    greenb = "#00b159";

    yellow = "#f18e38";
    yellowb = "#cc9c00";

    blue = "#428bca";
    blueb = "#428bca";

    magenta = "#B85C8A";
    magentab = "#B85C8A";

    cyan = "#669c9c";
    cyanb = "#669c9c";

    white = "#a89984";
    whiteb = "#f2efed";
  };
  solarized = {
    foreground = "#93a1a1";
    background = "#002b36";
    backgroundTRGBA = toTrgba "#002b36";
    backgroundTransparent = "[95]#002b36";
    cursorColor = "#800080";

    # black
    black = "#002b36";
    blackb = "#657b83";

    # red
    red = "#dc322f";
    redb = "#dc322f";

    # green
    green = "#859900";
    greenb = "#859900";

    # yellow
    yellow = "#b58900";
    yellowb = "#b58900";

    # blue
    blue = "#268bd2";
    blueb = "#268bd2";

    # magenta
    magenta = "#6c71c4";
    magentab = "#6c71c4";

    # cyan
    cyan = "#2aa198";
    cyanb = "#2aa198";

    # white
    white = "#93a1a1";
    whiteb = "#fdf6e3";
  };
  molokai = {
    foreground = "#cccccc";
    background = "#1b1d1e";

    black = "#1b1d1e";
    blackb = "#808080";

    red = "#f92672";
    redb = "#ff0044";

    green = "#a6e22e";
    greenb = "#82b414";

    yellow = "#e6db74";
    yellowb = "#fd971f";

    blue = "#7070f0";
    blueb = "#266c98";

    magenta = "#d63ae1";
    magentab = "#ac0cb1";

    cyan = "#66d9ef";
    cyanb = "#ae81ff";

    white = "#f8f8f2";
    whiteb = "#cccccc";

    backgroundTRGBA = "rgba(27, 29, 30, 0.95)";
    backgroundTransparent = "[95]#1b1d1e";
    cursorColor = "#800080";
  };
  dracula = rec {
    foreground = "#F8F8F2";
    background = "#282A36";

    black = "#000000";
    blackb = "#4D4D4D";

    red = "#FF5555";
    redb = "#FF6E67";

    green = "#50FA7B";
    greenb = "#5AF78E";

    yellow = "#F1FA8C";
    yellowb = "#F4F99D";

    blue = "#BD93F9";
    blueb = "#CAA9FA";

    magenta = "#FF79C6";
    magentab = "#FF92D0";

    cyan = "#8BE9FD";
    cyanb = "#9AEDFE";

    white = "#BFBFBF";
    whiteb = "#E6E6E6";

    backgroundTRGBA = toTrgba background;
    backgroundTransparent = "[95]${background}";
    cursorColor = "#008080";
  };
in { lib.colors = dracula; }

{ pkgs, ...}:
let
  hexToDec = import ./hexToDec.nix { inherit (pkgs) lib; };
  toTrgba = hex:
  let
    f = i: toString (hexToDec (builtins.substring i 2 hex));
    r = f 1;
    g = f 3;
    b = f 5;
  in "rgba(${r}, ${g}, ${b}, 0.95)";
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
in
  {
    lib.colors = solarized;
  }

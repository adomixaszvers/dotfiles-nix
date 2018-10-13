{ pkgs, ...}:
let
  solarized = {
    foreground = "#93a1a1";
    background = "#002b36";
    backgroundTRGBA = "rgba(0, 43, 54, 0.95)";
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
in
  {
    lib.colors.solarized = solarized;
  }

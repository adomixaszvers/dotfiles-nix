{ config, lib, inputs, ... }: {
  imports = [ inputs.nix-colors.homeManagerModules.default ];
  colors = let adaptColor = c: "#${lib.strings.toLower c}";
  in with config.colorScheme.palette; {
    foreground = adaptColor base05;
    background = adaptColor base00;
    cursorColor = adaptColor base05;

    black = adaptColor base00;
    blackb = adaptColor base03;

    red = adaptColor base08;
    redb = adaptColor base08;

    green = adaptColor base0B;
    greenb = adaptColor base0B;

    yellow = adaptColor base0A;
    yellowb = adaptColor base0A;

    blue = adaptColor base0D;
    blueb = adaptColor base0D;

    magenta = adaptColor base0E;
    magentab = adaptColor base0E;

    cyan = adaptColor base0C;
    cyanb = adaptColor base0C;

    white = adaptColor base05;
    whiteb = adaptColor base07;
  };
}

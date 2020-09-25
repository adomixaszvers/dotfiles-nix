{ pkgs, ... }:

{
  imports = [ ../picom.nix ../dunst.nix ];
  home.packages = let
    unstable = pkgs.nixos-unstable;
    myQtile = unstable.qtile.overridePythonAttrs (oldAttrs: {
      pythonPath = oldAttrs.pythonPath ++ [ unstable.python37Packages.xlib ];
    });
  in [ myQtile ];
  xsession.windowManager.command = "qtile";
  xdg.configFile."qtile/config.py" = {
    source = ./config.py;
    onChange = "qtile-cmd -o cmd -f restart 2>/dev/null || true";
  };
}

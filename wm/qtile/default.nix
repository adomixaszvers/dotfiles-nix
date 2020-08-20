{ pkgs, ... }:

{
  imports = [ ../picom.nix ../dunst.nix ];
  home.packages = let
    myQtile = pkgs.qtile.overridePythonAttrs (oldAttrs: {
      pythonPath = oldAttrs.pythonPath ++ [ pkgs.python37Packages.xlib ];
    });
  in [ myQtile ];
  xsession.windowManager.command = "qtile";
  xdg.configFile."qtile/config.py" = {
    source = ./config.py;
    onChange = "qtile-cmd -o cmd -f restart 2>/dev/null || true";
  };
}

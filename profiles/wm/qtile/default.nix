{ pkgs, ... }:

{
  imports = [ ../picom.nix ../dunst.nix ];
  home.packages = let
    unwrapped = pkgs.qtile.unwrapped.overrideAttrs (oldAttrs: {
      propagatedBuildInputs = oldAttrs.propagatedBuildInputs
        ++ [ pkgs.python3Packages.xlib ];
    });
    myQtile = (pkgs.python3.withPackages (_: [ unwrapped ])).overrideAttrs (_: {
      # otherwise will be exported as "env", this restores `nix search` behavior
      name = "${unwrapped.pname}-${unwrapped.version}";
      # export underlying qtile package
      passthru = { inherit unwrapped; };
    });

  in [ myQtile ];
  services.pasystray.enable = true;
  xsession.windowManager.command = "qtile start";
  xdg.configFile."qtile/config.py" = {
    source = ./config.py;
    # it tries to run a shellscript with python
    # onChange = "qtile cmd-obj -o cmd -f restart 2>/dev/null || true";
  };
}

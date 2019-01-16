{ pkgs, config}:
let
  colors = config.lib.colors.solarized;
  polybar = pkgs.polybar.override {
    i3GapsSupport = true;
    alsaSupport = true;
    pulseSupport = true;
  };
  scripts = import ./scripts { inherit pkgs polybar polybar-config; };
  polybar-config = import ./config.nix { inherit scripts pkgs colors; };
in
{
  launch = scripts.launch;
}

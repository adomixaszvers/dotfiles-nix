{ pkgs, config, ... }: {
  imports = [ ../polybar.nix ../dunst.nix ../compton.nix ];
  home.packages = with pkgs; [ bspwm mine.rofi-powermenu wmname tdrop ];
  services.polybar.config = let
    common = {
      modules-left = "bspwm";
      wm-restack = "bspwm";
    };
  in {
    "bar/top" = common;
    "bar/top-extra" = common;
    "module/bspwm" = let colors = config.lib.colors;
    in {
      type = "internal/bspwm";
      pin-workspaces = false;
      enable-click = true;

      format = "<label-state> <label-mode>";

      label-focused = "%name%";
      label-focused-overline = colors.foreground;

      label-occupied = "%name%";

      label-empty = "%name%";
      label-empty-foreground = colors.white;

      label-separator = " ";

      label-monocle = "";
      label-tiled = "";
      label-fullscreen = "";
      label-floating = "";
      label-pseudotiled = "P";
      label-locked = "";
      label-locked-foreground = "#bd2c40";
      label-sticky = "";
      label-sticky-foreground = "#fba922";
      label-private = "";
      label-private-foreground = "#bd2c40";
      label-marked = "M";
    };
  };
  xdg.configFile."bspwm/bspwmrc" = {
    source = ./bspwmrc;
    executable = true;
    onChange = "pidof bspwm 1>/dev/null && bspc wm -r";
  };
  xsession.windowManager.command = "bspwm";
}

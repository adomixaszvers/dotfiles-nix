{ pkgs, config, ... }: {
  imports = [ ../polybar.nix ../dunst.nix ../compton.nix ./sxhkd.nix ];
  home.packages = with pkgs; [ bspwm wmname tdrop ];
  services.polybar.config = let
    common = {
      modules-left = "bspwm divider title";
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

      label = "%{T2}%title%%{T-}";
      label-focused = "%{T2}%name%%{T-}";
      label-focused-overline = colors.foreground;

      label-occupied = "%{T2}%name%%{T-}";

      label-empty = "%name%";
      label-empty-foreground = colors.white;

      label-separator = " ";

      label-monocle = "%{T2}%{T-}";
      label-tiled = "%{T2}%{T-}";
      label-fullscreen = "%{T2}%{T-}";
      label-floating = "%{T2}%{T-}";
      label-pseudotiled = "%{T2}P%{T-}";
      label-locked = "%{T2}%{T-}";
      label-locked-foreground = "#bd2c40";
      label-sticky = "%{T2}%{T-}";
      label-sticky-foreground = "#fba922";
      label-private = "%{T2}%{T-}";
      label-private-foreground = "#bd2c40";
      label-marked = "%{T2}M%{T-}";
    };
  };
  xdg.configFile."bspwm/bspwmrc" = {
    source = ./bspwmrc;
    executable = true;
    onChange = "pidof bspwm 1>/dev/null && bspc wm -r";
  };
  xsession.windowManager.command = "bspwm";
}

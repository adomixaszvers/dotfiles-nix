{ pkgs, ... }: {
  home.packages = with pkgs; [ bspwm mine.sxhkd mine.rofi-powermenu wmname ];
  services.polybar.config = let
    common = {
      modules-left = "bspwm";
      wm-restack = "bspwm";
    };
  in {
    "bar/top" = common;
    "bar/top-extra" = common;
  };
  xdg.configFile."bspwm/bspwmrc" = {
    source = ./dotfiles/bspwmrc;
    executable = true;
    onChange = "pidof bspwm 1>/dev/null && bspc wm -r";
  };
  xdg.configFile."sxhkd/sxhkdrc" = {
    source = ./dotfiles/sxhkdrc;
    onChange = "pidof sxhkd 1>/dev/null && pkill -USR1 -x sxhkd";
  };
  xsession.windowManager.command = "bspwm";
}

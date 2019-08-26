{ pkgs, ... }: {
  home.packages = with pkgs; [ bspwm sxhkd ];
  services.polybar.config = {
    "bar/top".modules-left = "bspwm";
    "bar/top-extra".modules-left = "bspwm";
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

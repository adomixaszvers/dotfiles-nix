{ pkgs, ... }:
{
  home.packages = with pkgs; [ bspwm sxhkd ];
  xdg.configFile."bspwm/bspwmrc" = {
    source = ./dotfiles/bspwmrc;
    executable = true;
    onChange = "bspc wm -r";
  };
  xdg.configFile."sxhkd/sxhkdrc" = {
    source = ./dotfiles/sxhkdrc;
    onChange = "pkill -USR1 -x sxhkd";
  };
  xsession.windowManager.command = "bspwm";
}

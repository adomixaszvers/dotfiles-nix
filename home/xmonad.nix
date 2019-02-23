{pkgs, lib, config, ... }:
{
  home.packages = [ pkgs.xorg.xmessage ];
  xsession.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    config = ./dotfiles/xmonad.hs;
  };
}

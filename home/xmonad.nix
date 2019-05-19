{ pkgs, lib, config, ... }: {
  home.packages = with pkgs; [ xorg.xmessage xmobar ];
  xsession.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    config = ./dotfiles/xmonad.hs;
  };
}

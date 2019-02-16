{pkgs, config, ...}:
{
  home.packages = pkgs.lib.optionals
    config.xsession.windowManager.xmonad.enable [ pkgs.xorg.xmessage ];
  xsession.windowManager.xmonad = {
    enable = false;
    enableContribAndExtras = true;
    config = ~/.config/nixpkgs/home/dotfiles/xmonad.hs;
  };
}

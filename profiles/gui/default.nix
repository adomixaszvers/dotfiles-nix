{
  config,
  pkgs,
  lib,
  ...
}:

{
  imports = [
    ./alacritty
    ./chemacs
    ./doom-emacs.nix
    ./firefox.nix
    ./fonts.nix
    ./ghostty.nix
    ./kitty.nix
    ./stylix.nix
  ];
  home = {
    keyboard = {
      layout = "lt,us";
      options = [ "grp:caps_toggle" ];
    };
    file."vim-cheatsheet.png" = {
      source = pkgs.fetchurl {
        url = "http://i.imgur.com/YLInLlY.png";
        sha256 = "0qziky603gwbzjr8sjfmlxgnwsxmv5n7fvnygykm8xj2y43657xi";
      };
    };
    file."wallpaper.png".source = config.stylix.image;
    packages = with pkgs; [
      arandr
      font-manager
      adwaita-icon-theme
      hicolor-icon-theme
      meld
      pavucontrol
      pcmanfm
      qt5.qttools.bin
      vlc
      wmctrl
      xarchiver
      xsel
      zathura
    ];
    sessionVariables = {
      TERMINAL = "kitty";
    };
  };
  programs = {
    feh.enable = true;
    rofi = {
      enable = true;
      extraConfig = {
        modi = lib.mkDefault "drun,window,run,ssh";
        dpi = 1; # autodetect dpi based on monitor size
      };
    };
  };
  stylix.targets = {
    rofi.enable = true;
    xresources.enable = true;
  };
}

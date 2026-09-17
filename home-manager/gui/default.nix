{
  config,
  pkgs,
  lib,
  ...
}:

{
  imports = [
    ./alacritty
    ./doom-emacs.nix
    ./firefox.nix
    ./fonts.nix
    # ./ghostty.nix
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
    packages =
      builtins.attrValues {
        inherit (pkgs)
          # keep-sorted start
          adwaita-icon-theme
          arandr
          font-manager
          hicolor-icon-theme
          meld
          pavucontrol
          pcmanfm
          vlc
          wmctrl
          xarchiver
          xsel
          zathura
          # keep-sorted end
          ;
      }
      ++ [
        pkgs.qt5.qttools.bin
      ];
    sessionVariables = {
      TERMINAL = "kitty";
    };
  };
  programs = {
    feh.enable = true;
    rofi = {
      enable = true;
      settings = {
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

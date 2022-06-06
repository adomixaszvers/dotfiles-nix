{ config, pkgs, ... }:

{
  imports = [ ./alacritty ./chemacs ./doom-emacs.nix ./fonts.nix ./kitty.nix ];
  home.keyboard = {
    layout = "lt,us";
    options = [ "grp:caps_toggle" ];
  };
  home.file."vim-cheatsheet.png" = {
    source = pkgs.fetchurl {
      url = "http://i.imgur.com/YLInLlY.png";
      sha256 = "0qziky603gwbzjr8sjfmlxgnwsxmv5n7fvnygykm8xj2y43657xi";
    };
  };
  home.file."wallpaper.png" = {
    source = pkgs.fetchurl {
      url =
        "https://raw.githubusercontent.com/NixOS/nixos-artwork/master/wallpapers/nix-wallpaper-simple-light-gray.png";
      sha256 = "0i6d0xv1nzrv7na9hjrgzl3jrwn81vnprnq2pxyznlxbjcgkjnk2";
    };
  };
  home.packages = with pkgs; [
    arandr
    font-manager
    gnome.adwaita-icon-theme
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
  home.sessionVariables = { TERMINAL = "kitty"; };
  programs.feh.enable = true;
  programs.rofi = {
    enable = true;
    extraConfig = {
      modi = "drun,window,run,ssh";
      dpi = 1; # autodetect dpi based on monitor size
    };
    theme = "Pop-Dark";
  };
  services.gnome-keyring = {
    enable = true;
    components = [ "pkcs11" "secrets" "ssh" ];
  };
  xresources.properties = with config.colors; {

    # special
    "*.foreground" = foreground;
    "*.background" = background;
    "*.cursorColor" = cursorColor;

    # black
    "*.color0" = black;
    "*.color8" = blackb;

    # red
    "*.color1" = red;
    "*.color9" = redb;

    # green
    "*.color2" = green;
    "*.color10" = greenb;

    # yellow
    "*.color3" = yellow;
    "*.color11" = yellowb;

    # blue
    "*.color4" = blue;
    "*.color12" = blueb;

    # magenta
    "*.color5" = magenta;
    "*.color13" = magentab;

    # cyan
    "*.color6" = cyan;
    "*.color14" = cyanb;

    # white
    "*.color7" = white;
    "*.color15" = whiteb;
  };
}

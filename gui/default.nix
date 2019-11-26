{ config, lib, pkgs, ... }:

{
  imports = [
    ./alacritty
    ./colors.nix
    ./doom-emacs.nix
    ./fonts.nix
    ./keepass
    ./termite.nix
  ];
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
    evince
    font-manager
    gnome3.adwaita-icon-theme
    google-play-music-desktop-player
    hicolor-icon-theme
    i3lock
    meld
    pavucontrol
    pcmanfm
    vlc
    w3m # for ranger image previews
    wmctrl
    xarchiver
    xsel
  ];
  home.sessionVariables = { TERMINAL = "termite"; };
  programs.feh.enable = true;
  programs.rofi = {
    enable = true;
    extraConfig = ''
      rofi.modi: drun,window,run,ssh
    '';
    theme = "Pop-Dark";
  };
  services.gnome-keyring = {
    enable = true;
    components = [ "pkcs11" "secrets" "ssh" ];
  };
  xdg.configFile."ranger/rc.conf" = {
    text = ''
      set preview_images true
    '';
  };
  xresources.properties = { "Emacs.font" = "FuraMono Nerd Font Mono-9"; };
}

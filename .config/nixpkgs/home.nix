{ pkgs, ... }:
{
  imports = [ ./hosts/current-host.nix ];
  gtk = {
    enable = true;
    iconTheme = {
      name = "Numix";
      package = pkgs.numix-icon-theme;
    };
    theme = {
      name = "Numix";
      package = pkgs.numix-gtk-theme;
    };
  };
  home.keyboard.layout = "lt,us";
  home.file."wallpaper.png" = {
    source =  pkgs.fetchurl {
      url = "https://raw.githubusercontent.com/NixOS/nixos-artwork/d871f7cb0a39bb1d7c1370814ba13520d09fbc9d/wallpapers/nix-wallpaper-simple-dark-gray.png";
      sha256 = "1282cnqc5qynp0q9gdll7bgpw23yp5bhvaqpar59ibkh3iscg8i5";
    };
  };
  manual.html.enable = true;
  programs.home-manager.enable = true;
  programs.home-manager.path = https://github.com/rycee/home-manager/archive/release-18.03.tar.gz;
  programs.neovim = import ./neovim.nix { inherit pkgs; };
  programs.feh.enable = true;
  programs.rofi = {
    enable = true;
    theme = "solarized";
  };
  services.network-manager-applet.enable = true;
  xresources.extraConfig = ''
    URxvt*font: xft:Source Code Pro:size=10 
    URxvt.perl-ext-common: default,matcher,selection-to-clipboard,resize-font
    URxvt.url-launcher: /usr/bin/xdg-open
    URxvt.matcher.button: 1
    URxvt.letterSpace: -1
    URxvt.iso14755: false
    URxvt.iso14755_52: false
    URxvt.scrollBar:	false

    ! special
    *.foreground:   #93a1a1
    *.background:   #002b36
    *.cursorColor:  #93a1a1

    ! black
    *.color0:       #002b36
    *.color8:       #657b83

    ! red
    *.color1:       #dc322f
    *.color9:       #dc322f

    ! green
    *.color2:       #859900
    *.color10:      #859900

    ! yellow
    *.color3:       #b58900
    *.color11:      #b58900

    ! blue
    *.color4:       #268bd2
    *.color12:      #268bd2

    ! magenta
    *.color5:       #6c71c4
    *.color13:      #6c71c4

    ! cyan
    *.color6:       #2aa198
    *.color14:      #2aa198

    ! white
    *.color7:       #93a1a1
    *.color15:      #fdf6e3
  '';
  xsession.enable = true;
  xsession.windowManager.i3 = import ./i3.nix { inherit pkgs; };
}

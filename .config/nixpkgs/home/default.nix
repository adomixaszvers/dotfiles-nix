{ pkgs, ... }:
{
  imports = let
    current = ./hosts/current.nix;
    default = ./hosts/mini.nix;
    hostSpecific = if builtins.pathExists current then current else default;
  in
  [
    hostSpecific
    ./compton.nix
    ./dunst.nix
    ./i3.nix
    ./neovim.nix
    ./polybar.nix
    ./termite.nix
  ];
  gtk = {
    enable = true;
    iconTheme = {
      name = "Arc";
      package = pkgs.arc-icon-theme;
    };
    theme = {
      name = "Arc-Darker";
      package = pkgs.arc-theme;
    };
  };
  home.keyboard.layout = "lt,us";
  home.file."wallpaper.png" = {
    source =  pkgs.fetchurl {
      url = "http://i.imgur.com/YLInLlY.png";
      sha256 = "0qziky603gwbzjr8sjfmlxgnwsxmv5n7fvnygykm8xj2y43657xi";
    };
  };
  home.sessionVariables = {
    EDITOR = "nvim";
  };
  manual.html.enable = true;
  programs.autorandr = {
    enable = true;
    profiles = {
      work = {
        fingerprint = {
          VGA1 = "00ffffffffffff004c2daa085637555a021701030e301b782a90c1a259559c270e5054bfef80714f81c0810081809500a9c0b3000101023a801871382d40582c4500dd0c1100001e000000fd00384b1e5111000a202020202020000000fc00533232423330300a2020202020000000ff00484d42443130313132390a202000b4";
          eDP1 = "00ffffffffffff0006afec46000000000f15010490221378020bb59757548c2623505400000001010101010101010101010101010101ce1d56e250001e302616360058c110000018df1356e250001e302616360058c11000001800000000000000000000000000000000000000000002000d48ff0a3c64140e1a682020200039";
        };
        config = {
          HDMI1.enable = false;
          VIRTUAL1.enable = false;
          VGA1 = {
            enable = true;
            primary = true;
            position = "0x0";
            mode = "1920x1080";
            rate = "60.00";
          };
          eDP1 = {
            mode = "1366x768";
            position = "1920x0";
            rate = "60.06";
          };
        };
      };
    };
  };
  programs.fzf = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
  };
  programs.home-manager.enable = true;
  programs.home-manager.path = https://github.com/rycee/home-manager/archive/release-18.03.tar.gz;
  programs.feh.enable = true;
  programs.rofi = {
    enable = true;
    theme = "solarized";
  };
  programs.zsh.enable = true;
  services.network-manager-applet.enable = true;
  systemd.user.startServices = true;
  xresources.extraConfig = ''
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
  xsession.initExtra = "autorandr --change";
}

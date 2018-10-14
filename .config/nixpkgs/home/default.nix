{ pkgs, config, ... }:
{
  imports = let
    current = ./hosts/current.nix;
    default = ./hosts/mini.nix;
    hostSpecific = if builtins.pathExists current then current else default;
  in
  [
    hostSpecific
    ./compton.nix
    ./colors.nix
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
  home.file."vim-cheatsheet.png" = {
    source =  pkgs.fetchurl {
      url = "http://i.imgur.com/YLInLlY.png";
      sha256 = "0qziky603gwbzjr8sjfmlxgnwsxmv5n7fvnygykm8xj2y43657xi";
    };
  };
  home.file."wallpaper.png" = {
    source =  pkgs.fetchurl {
      url = "https://raw.githubusercontent.com/NixOS/nixos-artwork/master/wallpapers/nix-wallpaper-simple-light-gray.png";
      sha256 = "0i6d0xv1nzrv7na9hjrgzl3jrwn81vnprnq2pxyznlxbjcgkjnk2";
    };
  };
  home.file.".config/ranger/rc.conf" = {
    text = ''
      set preview_images true
    '';
  };
  home.sessionVariables = {
    EDITOR = "nvim";
    TERMINAL = "termite";
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
  programs.zsh = {
    enable = true;
    initExtra = ''
      prompt() {
        PS1="$(powerline-rs --shell zsh $?)"
      }
      precmd_functions+=(prompt)
    '';
  };
  services.network-manager-applet.enable = true;
  systemd.user.startServices = true;
  xresources.extraConfig = with config.lib.colors.solarized; ''
    URxvt*font: xft:SauceCodePro Nerd Font Mono:size=10
    URxvt.perl-ext-common: default,matcher,selection-to-clipboard,font-size
    URxvt.url-launcher: /usr/bin/xdg-open
    URxvt.matcher.button: 1
    URxvt.letterSpace: -1
    URxvt.iso14755: false
    URxvt.iso14755_52: false
    URxvt.scrollBar:	false
    URxvt.background: ${backgroundTransparent}
    urxvt*depth: 32

    URxvt.keysym.C-Up:     font-size:increase
    URxvt.keysym.C-Down:   font-size:decrease
    URxvt.keysym.C-S-Up:   font-size:incglobal
    URxvt.keysym.C-S-Down: font-size:decglobal
    URxvt.keysym.C-equal:  font-size:reset
    URxvt.keysym.C-slash:  font-size:show

    ! special
    *.foreground:   ${foreground}
    *.background:   ${background}
    *.cursorColor:  ${cursorColor}

    ! black
    *.color0:       ${black}
    *.color8:       ${blackb}

    ! red
    *.color1:       ${red}
    *.color9:       ${redb}

    ! green
    *.color2:       ${green}
    *.color10:      ${greenb}

    ! yellow
    *.color3:       ${yellow}
    *.color11:      ${yellowb}

    ! blue
    *.color4:       ${blue}
    *.color12:      ${blueb}

    ! magenta
    *.color5:       ${magenta}
    *.color13:      ${magentab}

    ! cyan
    *.color6:       ${cyan}
    *.color14:      ${cyanb}

    ! white
    *.color7:       ${white}
    *.color15:      ${whiteb}
  '';
  xsession.enable = true;
  xsession.initExtra = "autorandr --change";
}

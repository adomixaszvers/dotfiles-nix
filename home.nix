{ pkgs, config, lib, ... }: {
  imports = let
    current = ./home/hosts/current.nix;
    hostSpecific = if builtins.pathExists current then current else { };
  in [
    hostSpecific
    # ./home/awesome.nix
    # ./home/bspwm.nix
    ./home/compton.nix
    ./home/colors.nix
    ./home/dunst.nix
    # ./home/emacs.nix
    ./home/doom-emacs.nix
    # ./home/i3.nix
    ./home/keepass.nix
    ./home/neovim.nix
    ./home/alacritty.nix
    ./home/polybar.nix
    ./home/termite.nix
    ./home/xmonad.nix
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
  qt = {
    enable = true;
    platformTheme = "gtk";
  };
  home.keyboard = {
    layout = "lt,us";
    options = [ "ctrl:nocaps" ];
  };
  home.extraOutputsToInstall = [ "doc" "info" "devdoc" ];
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
  home.packages = with pkgs;
    let
      nixfmt = import <nixfmt> { };
      fonts = [
        corefonts
        emacs-all-the-icons-fonts
        google-fonts
        material-icons
        nerdfonts
      ];
    in fonts ++ [
      alacritty
      arandr
      bfs
      cachix
      dunst
      evince
      file
      font-manager
      git
      gnome3.adwaita-icon-theme
      google-play-music-desktop-player
      hicolor-icon-theme
      htop
      i3lock
      icdiff
      iotop
      kakoune
      lsof
      meld
      ncdu
      nixfmt
      p7zip
      pavucontrol
      pcmanfm
      ranger
      ripgrep
      rxvt_unicode-with-plugins
      shellcheck
      tree
      unzip
      w3m # for ranger image previews
      xarchiver
      xdg-user-dirs
      xsel
    ];
  home.sessionVariables = {
    EDITOR = "nvim";
    TERMINAL = "termite";
  };
  home.stateVersion = "19.03";
  manual.html.enable = true;
  programs.autorandr = {
    enable = true;
    profiles = {
      work = {
        fingerprint = {
          DP-1 =
            "00ffffffffffff0022f06e32010101012b1a0104a5342078224ca5a7554da226105054210800b30095008100d1c0a9c081c0a9408180283c80a070b023403020360006442100001a000000fd00323c1e5011010a202020202020000000fc00485020453234320a2020202020000000ff00434e43363433303832370a20200019";
          DP-2 =
            "00ffffffffffff0022f06e32010101010e1a0104a5342078224ca5a7554da226105054210800b30095008100d1c0a9c081c0a9408180283c80a070b023403020360006442100001a000000fd00323c1e5011010a202020202020000000fc00485020453234320a2020202020000000ff00434e433631343036364d0a20200020";
        };
        config = {
          HDMI1.enable = false;
          HDMI2.enable = false;
          VGA1.enable = false;
          VIRTUAL1.enable = false;
          DP-1 = {
            enable = true;
            position = "0x0";
            rate = "59.95";
            mode = "1920x1200";
          };
          DP-2 = {
            enable = true;
            position = "1920x0";
            primary = true;
            rate = "59.95";
            mode = "1920x1200";
          };
        };
        hooks.postswitch =
          "systemctl --user restart compton.service && i3-msg restart";
      };
    };
  };
  programs.bash = {
    enable = true;
    historyControl = [ "erasedups" "ignoredups" "ignorespace" ];
  };
  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
  };
  programs.fzf = {
    changeDirWidgetCommand = "bfs -type d";
    defaultCommand = "rg --files";
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
    fileWidgetCommand = "rg --files";
  };
  programs.home-manager.enable = true;
  # programs.home-manager.path = https://github.com/rycee/home-manager/archive/release-19.03.tar.gz;
  programs.feh.enable = true;
  programs.rofi = {
    enable = true;
    extraConfig = ''
      rofi.modi: drun,window,run,ssh
    '';
    theme = "Pop-Dark";
  };
  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = true;
    shellAliases = {
      he = "(hcd && nvim $(fzf))";
      hcd = "cd ~/.config/nixpkgs";
    };
  };
  services.gnome-keyring = {
    enable = true;
    components = [ "pkcs11" "secrets" "ssh" ];
  };
  services.network-manager-applet.enable = config.xsession.enable;
  systemd.user = {
    startServices = true;
    systemctlPath = "/run/current-system/sw/bin/systemctl";
  };

  xdg = {
    enable = true;
    # configFile."alacritty/alacritty.yml" = {
    #   text = import ./home/dotfiles/alacritty.yml.nix { inherit config; lib = pkgs.lib; };
    # };
    configFile."ranger/rc.conf" = {
      text = ''
        set preview_images true
      '';
    };
  };

  xresources.properties = with config.lib.colors; {
    "Emacs.font" = "FuraMono Nerd Font Mono-9";
    "URxvt*font" = "xft:SauceCodePro Nerd Font Mono:size=10";
    "URxvt.perl-ext-common" =
      "default,matcher,selection-to-clipboard,font-size";
    "URxvt.url-launcher" = "/usr/bin/xdg-open";
    "URxvt.matcher.button" = "1";
    "URxvt.letterSpace" = "-1";
    "URxvt.iso14755" = "false";
    "URxvt.iso14755_52" = "false";
    "URxvt.scrollBar" = "false";
    "URxvt.background" = background;
    "urxvt*depth" = "32";

    "URxvt.keysym.C-Up" = "font-size:increase";
    "URxvt.keysym.C-Down" = "font-size:decrease";
    "URxvt.keysym.C-S-Up" = "font-size:incglobal";
    "URxvt.keysym.C-S-Down" = "font-size:decglobal";
    "URxvt.keysym.C-equal" = "font-size:reset";
    "URxvt.keysym.C-slash" = "font-size:show";

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
  xsession.enable = lib.mkDefault true;
  xsession.initExtra = ''
    autorandr --change
    unset SSH_ASKPASS
    xset s off -dpms
  '';
  xsession.pointerCursor = {
    name = "capitaine-cursors";
    package = pkgs.capitaine-cursors;
  };
}

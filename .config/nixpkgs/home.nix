{ pkgs, ... }:
{
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
  home.packages = with pkgs; [
    (callPackage ./consul {})
    (callPackage ./vimgolf {})
    (ghc.withPackages (
    haskellPackages: with haskellPackages; [
        # ghc-mod
        # leksah
        hasktags
        hdevtools
        hindent
        hlint
        hoogle
        hspec
        pointfree pointful
        stylish-haskell
      ]))
    (python3.withPackages (
    pythonPackages: with pythonPackages; [
      i3ipc requests taskw psutil
    ]))
    arandr
    atom
    calibre
    discord
    exercism
    file
    git
    gnome3.adwaita-icon-theme
    gnome3.gnome-screenshot
    google-chrome
    htop
    jetbrains.idea-ultimate
    keepass
    klavaro
    lxappearance
    meld
    ncdu
    nodejs
    notify-osd
    okular
    qbittorrent
    ranger
    # skype
    smartgithg
    stack
    thefuck
    torbrowser
    tree
    # typora
    vcsh
    viber
    vim
    vlc
    xfce.gvfs
    xfce.thunar-bare
  ];
  programs.home-manager.enable = true;
  programs.home-manager.path = https://github.com/rycee/home-manager/archive/release-18.03.tar.gz;
  programs.neovim = {
    enable = true;
    configure = {
      customRC = ''
          set number
          set relativenumber
          set path+=**
          set list

          let g:neomake_open_list = 2

          colorscheme google
          packadd neomake
          call neomake#configure#automake('nrwi', 500)
          let g:hardtime_default_on = 0
      '';
      packages.myNeoVim = with pkgs.vimPlugins; {
        start = [
          tlib
          surround
          vim-addon-actions
          vim-addon-completion
          vim-addon-errorformats
          vim-addon-goto-thing-at-cursor
          vim-addon-mw-utils
          vim-addon-nix
          vim-colorschemes
          vim-hardtime
          vim-hdevtools
          vim-nix
        ];
        opt = [ neomake ];
      };
    };
  };
  programs.feh.enable = true;
  programs.rofi = {
    enable = true;
    colors = {
      window = {
        background = "argb:ee222222";
        border = "#FAC863 ";
        separator = "#FAC863";
      };
      rows = {
        active = {
          background = "argb:00000000";
          foreground = "#6699CC";
          backgroundAlt = "argb:00000000";
          highlight = {
            background = "#6699CC";
            foreground = "#1B2B34";
          };
        };
        normal = {
          background = "argb:00000000";
          foreground = "#D8DEE9";
          backgroundAlt = "argb:00000000";
          highlight = {
            background = "#FAC863";
            foreground = "#1B2B34";
          };
        };
        urgent = {
          background = "argb:00000000";
          foreground = "#F99157";
          backgroundAlt = "argb:00000000";
          highlight = {
            background = "#F99157";
            foreground = "#1B2B34";
          };
        };
      };
    };
    extraConfig = ''
      rofi.separator-style:                solid.color15: #c5c5c5
    '';
  };
}

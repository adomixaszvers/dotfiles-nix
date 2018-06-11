{
  allowUnfree = true;
  vim.ftNix = false;
  packageOverrides = pkgs: with pkgs; rec {
    myNeoVim = neovim.override {
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
          let g:hardtime_default_on = 1
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
    all = let
      myHaskellPackages = haskellPackages: with haskellPackages; [
        hasktags
        hdevtools
        hindent
        hlint
        hoogle
        hspec
        pointfree pointful
        stylish-haskell
      ];
    in with pkgs; buildEnv {
      name = "all";
      paths = [
        # jetbrains.idea-ultimate
        (callPackage ./consul {})
        (callPackage ./vimgolf {})
        (ghc.withPackages myHaskellPackages)
        (import ./npm-neovim {}).package
        arandr
        atom
        calibre
        exercism
        feh
        file
        gnome3.gnome-screenshot
        google-chrome
        htop
        keepass
        lxappearance
        myNeoVim
        ncdu
        nodejs
        notify-osd
        numix-gtk-theme
        numix-icon-theme
        ranger
        rofi
        skypeforlinux
        stack
        thefuck
        torbrowser
        tree
        vcsh
        viber
        vim
        xfce.thunar-bare
      ];
    };
  };
}

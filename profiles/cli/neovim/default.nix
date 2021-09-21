{ pkgs, lib, ... }:
let
  inherit (pkgs.nixos-unstable) neovim vimPlugins tree-sitter;
  myNeovim = neovim.override {
    configure = {
      packages.myPackageDir.start = with vimPlugins;
        let telescope-dependencies = [ plenary-nvim telescope-nvim ];
        in [
          ale
          commentary
          fugitive
          neoformat
          (nvim-treesitter.withPlugins (_: tree-sitter.allGrammars))
          playground
          rainbow
          repeat
          suda-vim
          surround
          vim-airline
          vim-airline-themes
          vim-colorschemes
          vim-gitgutter
          vim-polyglot
          vim-sneak
          vim-unimpaired
          vinegar
        ] ++ telescope-dependencies;
      customRC = lib.readFile ./init.vim + lib.readFile ./playground.vim;
    };
  };
in { home.packages = [ myNeovim ]; }

{ pkgs, lib, ... }:
let
  inherit (pkgs.nixos-unstable)
    vimPlugins tree-sitter neovim-unwrapped wrapNeovimUnstable neovimUtils;
  myNeovimConfig = neovimUtils.makeNeovimConfig {
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
    wrapRc = false;
  };
  myNeovim = wrapNeovimUnstable neovim-unwrapped myNeovimConfig;
in {
  home.packages = [ myNeovim ];
  xdg.configFile."nvim/init.vim".text = myNeovimConfig.neovimRcContent;
}

{ pkgs, unstable, ... }: {
  programs.neovim = {
    enable = true;
    package = unstable.neovim-unwrapped;
    plugins = with unstable.vimPlugins;
      let telescope-dependencies = [ plenary-nvim telescope-nvim ];
      in [
        ale
        commentary
        fugitive
        neoformat
        (nvim-treesitter.withPlugins (_: pkgs.tree-sitter.allGrammars))
        {
          plugin = nvim-lspconfig;
          config = builtins.readFile ./lspconfig.vim;
        }
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
    extraConfig = builtins.readFile ./init.vim
      + builtins.readFile ./playground.vim;
  };
}

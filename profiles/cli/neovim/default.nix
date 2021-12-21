{ pkgs, lib, ... }: {
  programs.neovim = {
    enable = true;
    package = pkgs.nixos-unstable.neovim-unwrapped;
    plugins = with pkgs.nixos-unstable.vimPlugins;
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
    extraConfig = lib.readFile ./init.vim + lib.readFile ./playground.vim;
  };
}

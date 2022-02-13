pkgs:
with pkgs.vimPlugins;
let telescope-dependencies = [ plenary-nvim telescope-nvim ];
in [
  ale
  commentary
  fugitive
  neoformat
  {
    plugin = nvim-treesitter.withPlugins (_: pkgs.tree-sitter.allGrammars);
    config = builtins.readFile ./nvim-treesitter.vim;
  }
  {
    plugin = nvim-lspconfig;
    config = builtins.readFile ./lspconfig.vim;
  }
  {
    plugin = playground;
    config = builtins.readFile ./playground.vim;
  }
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
] ++ telescope-dependencies

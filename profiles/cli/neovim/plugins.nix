pkgs:
with pkgs.vimPlugins;
let
  telescope-dependencies = [
    plenary-nvim
    telescope-nvim
    telescope-fzf-native-nvim
  ];
  cmp-dependencies = [
    cmp-nvim-lsp
    cmp-buffer
    cmp-path
    cmp-cmdline
    nvim-cmp
    vim-vsnip
  ];
in
[
  ale
  commentary
  direnv-vim
  fidget-nvim
  fugitive
  gitsigns-nvim
  {
    plugin = lualine-nvim;
    optional = true;
  }
  neoformat
  {
    plugin = nvim-web-devicons;
    optional = true;
  }
  nordic-nvim
  {
    plugin = nvim-treesitter.withAllGrammars;
    # config = builtins.readFile ./nvim-treesitter.vim;
  }
  {
    plugin = nvim-lspconfig;
    # config = builtins.readFile ./lspconfig.vim;
  }
  {
    plugin = playground;
    # config = builtins.readFile ./playground.vim;
  }
  rainbow
  repeat
  solarized
  vim-suda
  surround
  vim-polyglot
  vim-sneak
  vim-unimpaired
  vinegar
  which-key-nvim
  yuck-vim
]
++ telescope-dependencies
++ cmp-dependencies

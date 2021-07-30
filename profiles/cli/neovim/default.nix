{ pkgs, lib, ... }:
let inherit (pkgs.nixos-unstable) neovim-unwrapped vimPlugins tree-sitter;
in {
  programs.neovim = {
    enable = true;
    package = neovim-unwrapped;
    extraConfig = lib.readFile ./vimrc;
    plugins = with vimPlugins; [
      ale
      commentary
      fugitive
      fzf-vim
      fzfWrapper
      neoformat
      {
        plugin = nvim-treesitter.withPlugins (_: tree-sitter.allGrammars);
        config = ''
          lua <<EOF
            require'nvim-treesitter.configs'.setup {
              highlight = {
                enable = true,              -- false will disable the whole extension
              },
              incremental_selection = {
                enable = true,
                keymaps = {
                  init_selection = "<leader>gn",
                  node_incremental = "grn",
                  scope_incremental = "grc",
                  node_decremental = "grm",
                },
              },
            }
          EOF
        '';
      }
      {
        plugin = playground;
        config = builtins.readFile ./playground.vim;
      }
      rainbow
      repeat
      {
        plugin = suda-vim;
        config = "let g:suda_smart_edit = 1";
      }
      surround
      vim-airline
      vim-airline-themes
      vim-colorschemes
      vim-gitgutter
      vim-polyglot
      vim-sneak
      vim-unimpaired
      vinegar
    ];
  };
}

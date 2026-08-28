{ pkgs, ... }:
{
  specs = {
    general = {
      data = builtins.attrValues {
        inherit (pkgs.vimPlugins)
          vim-commentary
          conform-nvim
          vim-fugitive
          fzf-lua
          gitsigns-nvim
          lazydev-nvim
          nvim-sops
          # rainbow-delimiters-nvim
          vim-repeat
          vim-suda
          vim-surround
          vim-sneak
          vim-unimpaired
          vim-vinegar
          which-key-nvim
          yuck-vim
          ;
      };
      runtimePkgs = builtins.attrValues {
        inherit (pkgs)
          nixfmt
          ripgrep
          deadnix
          statix
          stylua
          ;
      };
    };
    lazy = {
      lazy = true;
      data = builtins.attrValues {
        inherit (pkgs.vimPlugins)
          lualine-nvim
          fidget-nvim
          nvim-web-devicons
          ;
      };
    };
    themer = {
      data = builtins.attrValues {
        inherit (pkgs.vimPlugins)
          catppuccin-nvim
          vim-colors-solarized
          ;
      };
    };
    blink-cmp = {
      data = [
        pkgs.vimPlugins.blink-cmp
      ];
    };
  };
}

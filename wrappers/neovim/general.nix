{ pkgs, ... }:
{
  specs = {
    general = {
      data = with pkgs.vimPlugins; [
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
      ];
      runtimePkgs = with pkgs; [
        nixfmt
        ripgrep
        deadnix
        statix
        stylua
      ];
    };
    lazy = {
      lazy = true;
      data = with pkgs.vimPlugins; [
        lualine-nvim
        fidget-nvim
        nvim-web-devicons
      ];
    };
    themer = {
      data = with pkgs.vimPlugins; [
        catppuccin-nvim
        vim-colors-solarized
      ];
    };
    blink-cmp = {
      data = [
        pkgs.vimPlugins.blink-cmp
      ];
    };
  };
}

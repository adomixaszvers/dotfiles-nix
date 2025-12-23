{
  categoryDefinitions =
    {
      pkgs,
      # settings,
      # categories,
      # extra,
      # name,
      # mkNvimPlugin,
      ...
    }:
    {
      lspsAndRuntimeDeps = {
        general = with pkgs; [
          nixfmt-rfc-style
          ripgrep
          deadnix
          statix
          stylua
        ];
        lsp = with pkgs; [
          lua-language-server
          nil
          taplo
        ];
      };
      startupPlugins = {
        general = with pkgs.vimPlugins; [
          ale
          commentary
          conform-nvim
          fugitive
          fzf-lua
          gitsigns-nvim
          nvim-sops
          # rainbow-delimiters-nvim
          repeat
          vim-suda
          surround
          vim-sneak
          vim-unimpaired
          vinegar
          which-key-nvim
          yuck-vim
        ];
        extra = with pkgs.vimPlugins; [
          direnv-vim
        ];
        themer = with pkgs.vimPlugins; [
          nordic-nvim
          solarized
        ];
        lsp = [ pkgs.vimPlugins.nvim-lspconfig ];
        blink-cmp = [
          pkgs.vimPlugins.blink-cmp
        ];
        cmp = with pkgs.vimPlugins; [
          cmp-nvim-lsp
          cmp-buffer
          cmp-path
          cmp-cmdline
          nvim-cmp
          vim-vsnip
        ];
        treesitter-full = [ pkgs.vimPlugins.nvim-treesitter.withAllGrammars ];
        treesitter-small = [
          (pkgs.vimPlugins.nvim-treesitter.withPlugins (
            p: with p; [
              bash
              c
              lua
              nix
              vim
            ]
          ))
        ];
      };
      optionalPlugins = {
        general = with pkgs.vimPlugins; [
          lualine-nvim
          fidget-nvim
          nvim-web-devicons
        ];
      };
    };
  packageDefinitions = {
    # the name here is the name of the package
    # and also the default command name for it.
    nixCats = _: {
      # these also recieve our pkgs variable
      # see :help nixCats.flake.outputs.packageDefinitions
      settings = {
        aliases = [ "nvim" ];
        # explained below in the `regularCats` package's definition
        # OR see :help nixCats.flake.outputs.settings for all of the settings available
        wrapRc = true;
        hosts = {
          python3.enable = false;
          node.enable = false;
          ruby.enable = false;
        };
      };
      # enable the categories you want from categoryDefinitions
      categories = {
        general = true;
        themer = true;
        lsp = true;
        blink-cmp = true;
        cmp = false;
        extra = true;
        treesitter-full = true;
        treesitter-small = false;
      };
    };
    nixCats-small = _: {
      # these also recieve our pkgs variable
      # see :help nixCats.flake.outputs.packageDefinitions
      settings = {
        aliases = [ "nvim" ];
        # explained below in the `regularCats` package's definition
        # OR see :help nixCats.flake.outputs.settings for all of the settings available
        wrapRc = true;
        hosts = {
          python3.enable = false;
          node.enable = false;
          ruby.enable = false;
        };
      };
      # enable the categories you want from categoryDefinitions
      categories = {
        general = true;
        themer = true;
        extra = false;
        lsp = false;
        blink-cmp = true;
        cmp = false;
        treesitter-full = false;
        treesitter-small = true;
      };
    };
  };
}

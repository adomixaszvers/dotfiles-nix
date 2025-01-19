{ pkgs, inputs }:
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
    }@packageDef:
    {
      lspsAndRuntimeDeps = {
        general = with pkgs; [
          nixfmt
          ripgrep
          deadnix
          statix
        ];
      };
      startupPlugins = {
        general = with pkgs.vimPlugins; [
          ale
          commentary
          direnv-vim
          fugitive
          gitsigns-nvim
          neoformat
          nvim-sops
          nordic-nvim
          playground
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
        ];
        themer = with pkgs.vimPlugins; [
          nordic-nvim
          solarized
        ];
        lsp = [ pkgs.vimPlugins.nvim-lspconfig ];
        telescope = with pkgs.vimPlugins; [
          plenary-nvim
          telescope-nvim
          telescope-fzf-native-nvim
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
          pkgs.vimPlugins.nvim-treesitter.withPlugins
          (
            p: with p; [
              bash
              c
              lua
              nix
              vim
            ]
          )
        ];
      };
      optionalPlugins = {
        general = with pkgs.vimPlugins; [
          lualine-nvim
          fidget-nvim
          nvim-web-devicons
        ];
      };
      # shared libraries to be added to LD_LIBRARY_PATH
      # variable available to nvim runtime
      sharedLibraries = {
        general = with pkgs; [
          # libgit2
        ];
      };
      environmentVariables = {
        # test = {
        #   CATTESTVAR = "It worked!";
        # };
      };
      extraWrapperArgs = {
        # test = [
        #   ''--set CATTESTVAR2 "It worked again!"''
        # ];
      };
      # lists of the functions you would have passed to
      # python.withPackages or lua.withPackages

      # get the path to this python environment
      # in your lua config via
      # vim.g.python3_host_prog
      # or run from nvim terminal via :!<packagename>-python3
      extraPython3Packages = {
        test = _: [ ];
      };
      # populates $LUA_PATH and $LUA_CPATH
      extraLuaPackages = {
        test = [ (_: [ ]) ];
      };
    };
     packageDefinitions = {
      # the name here is the name of the package
      # and also the default command name for it.
      nixCats = { pkgs, ... }@misc: {
        # these also recieve our pkgs variable
        # see :help nixCats.flake.outputs.packageDefinitions
        settings = {
          aliases = [];

          # explained below in the `regularCats` package's definition
          # OR see :help nixCats.flake.outputs.settings for all of the settings available
          wrapRc = true;
          configDirName = "nixCats-nvim";
          # neovim-unwrapped = inputs.neovim-nightly-overlay.packages.${pkgs.system}.neovim;
        };
        # enable the categories you want from categoryDefinitions
        categories = {
          general = true;
          themer = true;
          lsp = true;
          cmp = true;
          telescope = true;
          treesitter-full = true;
          treesitter-small = false;
        };
      };
    };
}

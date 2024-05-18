{ neovimUtils, wrapNeovimUnstable, neovim-unwrapped, vimPlugins, lib, nixfmt
, ripgrep, statix, deadnix }:
let
  plugins = with vimPlugins;
    let
      telescope-dependencies =
        [ plenary-nvim telescope-nvim telescope-fzf-native-nvim ];
    in [
      ale
      commentary
      fugitive
      lualine-nvim
      neoformat
      nordic-nvim
      (nvim-treesitter.withPlugins (p: with p; [ bash c lua nix vim ]))
      repeat
      suda-vim
      surround
      vim-gitgutter
      vim-polyglot
      vim-sneak
      vim-unimpaired
      vinegar
      which-key-nvim
    ] ++ telescope-dependencies;
  moduleConfigure = {
    packages.neovim-nix = {
      start = plugins;
      opt = [ ];
    };
    beforePlugins = "";
  };

  neovimConfig = neovimUtils.makeNeovimConfig {
    inherit plugins;
    withPython3 = false;
    withNode = false;
    withRuby = false;
    configure = moduleConfigure;
    customRC = ''
      luafile ${./nvim-treesitter.lua}
      luafile ${./init.lua}
    '';
  };
  extraWrapperArgs = let extraPackages = [ nixfmt ripgrep deadnix statix ];
  in ''--suffix PATH : "${lib.makeBinPath extraPackages}"'';
in wrapNeovimUnstable neovim-unwrapped (neovimConfig // {
  wrapperArgs = (lib.escapeShellArgs neovimConfig.wrapperArgs) + " "
    + extraWrapperArgs;
})

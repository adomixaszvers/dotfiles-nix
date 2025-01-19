{
  neovimUtils,
  wrapNeovimUnstable,
  neovim-unwrapped,
  vimPlugins,
  lib,
  nixfmt,
  ripgrep,
  statix,
  deadnix,
}:
let
  plugins =
    with vimPlugins;
    let
      telescope-dependencies = [
        plenary-nvim
        telescope-nvim
        telescope-fzf-native-nvim
      ];
    in
    [
      ale
      commentary
      fidget-nvim
      fugitive
      gitsigns-nvim
      lualine-nvim
      nvim-web-devicons
      neoformat
      nordic-nvim
      (nvim-treesitter.withPlugins (
        p: with p; [
          bash
          c
          lua
          nix
          vim
        ]
      ))
      repeat
      vim-suda
      surround
      vim-polyglot
      vim-sneak
      vim-unimpaired
      vinegar
      which-key-nvim
    ]
    ++ telescope-dependencies;
  normalizedPlugins = neovimUtils.normalizePlugins plugins;
  moduleConfigure = {
    packages.neovim-nix = neovimUtils.normalizedPluginsToVimPackage normalizedPlugins;
    beforePlugins = "";
  };

  neovimConfig = neovimUtils.makeNeovimConfig {
    inherit plugins;
    withPython3 = false;
    withNode = false;
    withRuby = false;
    configure = moduleConfigure;
    luaRcContent = # lua
      ''
        dofile '${./lua/nvim-treesitter.lua}'
        dofile '${./lua/init.lua}'
      '';
  };
  extraWrapperArgs =
    let
      extraPackages = [
        nixfmt
        ripgrep
        deadnix
        statix
      ];
    in
    ''--suffix PATH : "${lib.makeBinPath extraPackages}"'';
in
wrapNeovimUnstable neovim-unwrapped (
  neovimConfig
  // {
    wrapperArgs = (lib.escapeShellArgs neovimConfig.wrapperArgs) + " " + extraWrapperArgs;
  }
)

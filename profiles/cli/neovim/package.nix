{
  neovimUtils,
  wrapNeovimUnstable,
  neovim-unwrapped,
  pkgs,
}:
let
  plugins = import ./plugins.nix pkgs;
  normalizedPlugins = neovimUtils.normalizePlugins plugins;
  moduleConfigure = {
    packages.home-manager = neovimUtils.normalizedPluginsToVimPackage normalizedPlugins;
    beforePlugins = "";
  };

  neovimConfig = neovimUtils.makeNeovimConfig {
    inherit plugins;
    configure = moduleConfigure;
    luaRcContent = import ./customRc.nix;
  };
in
wrapNeovimUnstable neovim-unwrapped neovimConfig

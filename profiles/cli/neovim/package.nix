{ neovimUtils, lib, wrapNeovimUnstable, neovim-unwrapped, pkgs }:
let
  plugins = import ./plugins.nix pkgs;
  moduleConfigure = {
    packages.home-manager = {
      start = lib.remove null (map
        (x: if x ? plugin && x.optional or false then null else (x.plugin or x))
        plugins);
      opt = lib.remove null
        (map (x: if x ? plugin && x.optional or false then x.plugin else null)
          plugins);
    };
    beforePlugins = "";
  };

  neovimConfig = neovimUtils.makeNeovimConfig {
    inherit plugins;
    configure = moduleConfigure;
    customRC = builtins.readFile ./init.vim;
  };
in wrapNeovimUnstable neovim-unwrapped neovimConfig

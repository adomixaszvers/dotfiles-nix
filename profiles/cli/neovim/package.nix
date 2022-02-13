{ neovimUtils, wrapNeovimUnstable, neovim-unwrapped, pkgs }:
let
  plugins = import ./plugins.nix pkgs;
  moduleConfigure = {
    packages.home-manager = let
      ps = builtins.partition (x: x.optional or false)
        (map (x: x.plugin or x) plugins);
    in {
      start = ps.wrong;
      opt = ps.right;
    };
    beforePlugins = "";
  };

  neovimConfig = neovimUtils.makeNeovimConfig {
    inherit plugins;
    configure = moduleConfigure;
    customRC = builtins.readFile ./init.vim;
  };
in wrapNeovimUnstable neovim-unwrapped neovimConfig

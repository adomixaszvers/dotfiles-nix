{
  inputs,
  lib,
  config,
  ...
}:
{
  imports = [ inputs.nixCats.homeModule ];
  stylix.targets.neovim.enable = true;
  nixCats =
    let
      inherit (import ./nixCats.nix) categoryDefinitions packageDefinitions;
    in
    {
      enable = true;
      luaPath = builtins.path {
        name = "my-neovim-config";
        path = ./luapath;
      };
      packageNames = lib.mkDefault [ "nixCats-stylix" ];
      categoryDefinitions.replace = config.nixCats.utils.mergeCatDefs categoryDefinitions (
        { pkgs, ... }:
        {
          startupPlugins.stylix = [
            {
              plugin = pkgs.vimPlugins.mini-base16;
              type = "lua";
              config = config.stylix.targets.neovim.pluginColorConfig;
            }
          ];
        }
      );
      packageDefinitions.replace = packageDefinitions // {
        nixCats-stylix = config.nixCats.utils.mergeCatDefs packageDefinitions.nixCats (_: {
          categories = {
            stylix = true;
            themer = false;
          };
        });
        nixCats-small-stylix = config.nixCats.utils.mergeCatDefs packageDefinitions.nixCats-small (_: {
          categories = {
            stylix = true;
            themer = false;
          };
        });
      };
    };
}

{ inputs, lib, ... }:
{
  imports = [ inputs.nixCats.homeModule ];
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
      packageNames = lib.mkDefault [ "nixCats" ];
      categoryDefinitions.replace = categoryDefinitions;
      packageDefinitions.replace = packageDefinitions;
    };
}

{ inputs, ... }:
{
  imports = [ inputs.nixCats.homeModule ];
  nixCats =
    let
      inherit (import ./nixCats.nix) categoryDefinitions packageDefinitions;
    in
    {
      enable = true;
      luaPath = ./luapath;
      packageNames = [ "nvim" ];
      categoryDefinitions.replace = categoryDefinitions;
      packageDefinitions.replace = packageDefinitions;
    };
}

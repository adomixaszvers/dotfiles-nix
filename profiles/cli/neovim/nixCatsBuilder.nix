{
  nixpkgs,
  nixCats,
  system,
}:
let
  inherit (import ./nixCats.nix) categoryDefinitions packageDefinitions;
  luaPath = ./luapath;
  inherit (nixCats) utils;
  nixCatsBuilder = utils.baseBuilder luaPath {
    inherit nixpkgs system;
  } categoryDefinitions packageDefinitions;
in
nixCatsBuilder

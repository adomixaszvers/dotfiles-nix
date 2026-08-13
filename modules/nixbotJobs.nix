{ lib, flake-parts-lib, ... }:
let
  inherit (lib)
    mkOption
    types
    ;
  inherit (flake-parts-lib)
    mkTransposedPerSystemModule
    ;
in
mkTransposedPerSystemModule {
  name = "nixbotJobs";
  option = mkOption {
    type = types.lazyAttrsOf types.package;
    default = { };
    description = ''
      An attribute set of packages to be built by nixbot
    '';
  };
  file = ./nixbotJobs.nix;
}

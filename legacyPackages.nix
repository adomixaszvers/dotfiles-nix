{ self, ... }:
{

  perSystem =
    {
      self',
      lib,
      system,
      ...
    }:
    {
      # make pkgs available to all `perSystem` functions
      legacyPackages =
        let
          nixosMachines =
            lib.mapAttrs' (name: config: lib.nameValuePair "nixos-${name}" config.config.system.build.toplevel)
              (
                (lib.filterAttrs (_: config: config.pkgs.stdenv.hostPlatform.system == system))
                  self.nixosConfigurations
              );
          blacklistPackages = [
          ];
          packages = lib.mapAttrs' (n: lib.nameValuePair "package-${n}") (
            lib.filterAttrs (n: _v: !(builtins.elem n blacklistPackages)) self'.packages
          );
          devShells = lib.mapAttrs' (n: lib.nameValuePair "devShell-${n}") self'.devShells;
          homeConfigurations = lib.mapAttrs' (
            name: config: lib.nameValuePair "home-manager-${name}" config.activation-script
          ) { };
        in
        nixosMachines // packages // devShells // homeConfigurations;

    };
}

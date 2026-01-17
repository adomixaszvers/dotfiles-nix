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
      legacyPackages =
        let
          systemFilter = lib.filterAttrs (_: config: config.pkgs.stdenv.hostPlatform.system == system);
          nixosMachines = lib.mapAttrs' (
            name: config: lib.nameValuePair "nixos-${name}" config.config.system.build.toplevel
          ) (systemFilter self.nixosConfigurations);
          blacklistPackages = [
          ];
          packages = lib.mapAttrs' (n: lib.nameValuePair "package-${n}") (
            lib.filterAttrs (n: _v: !(builtins.elem n blacklistPackages)) self'.packages
          );
          devShells = lib.mapAttrs' (n: lib.nameValuePair "devShell-${n}") self'.devShells;
        in
        nixosMachines // packages // devShells;

    };
}

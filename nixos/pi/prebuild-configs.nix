{ config, pkgs, ... }:
{
  systemd = {
    services.prebuild-configs = {
      path = [
        config.nix.package
        pkgs.cachix
      ];
      serviceConfig.User = "pi";
      description = "Prebuild personal configs";
      script =
        let
          flakeRef = "github:adomixaszvers/dotfiles-nix/update_flake_lock_action";
        in
        # bash
        ''
          nix build --no-link --keep-going '${flakeRef}#nixosConfigurations.raspberrypi-nixos.config.system.build.toplevel' '${flakeRef}#homeConfigurations.pi.activationPackage' '${flakeRef}#devShells.aarch64-linux.default'| cachix push adomixaszvers
        '';
      startAt = "Fri, 06:00";
    };
  };
}

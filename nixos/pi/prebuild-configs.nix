{ config, ... }:
{
  systemd = {
    services.prebuild-configs = {
      path = [ config.nix.package ];
      serviceConfig.User = "pi";
      description = "Prebuild personal configs";
      script =
        let
          flakeRef = "github:adomixaszvers/dotfiles-nix/update_flake_lock_action";
        in
        # bash
        ''
          nix flake archive --refresh '${flakeRef}'
          nix build --no-link --keep-going '${flakeRef}#nixosConfigurations.raspberrypi-nixos.config.system.build.toplevel' '${flakeRef}#homeConfigurations.pi.activationPackage' '${flakeRef}#devShells.aarch64-linux.default'
        '';
      startAt = "Fri, 06:00";
    };
  };
}

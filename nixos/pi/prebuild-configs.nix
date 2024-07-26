{ config, lib, ... }:
{
  systemd = {
    services.prebuild-configs = {
      serviceConfig.User = "pi";
      description = "Prebuild personal configs";
      script =
        let
          nix = config.nix.package;
          flakeRef = "github:adomixaszvers/dotfiles-nix/update_flake_lock_action";
        in
        # bash
        ''
          PATH=${lib.makeBinPath [ nix ]}:$PATH
          nix flake archive --refresh '${flakeRef}'
          nix build --no-link '${flakeRef}#nixosConfigurations.raspberrypi-nixos.config.system.build.toplevel' '${flakeRef}#homeConfigurations.pi.activationPackage' '${flakeRef}#devShells.aarch64-linux.default'
        '';
      startAt = "Fri, 06:00";
    };
  };
}

{ config, lib, ... }:
{
  systemd = {
    services.prebuild-configs = {
      serviceConfig.User = "pi";
      description = "Prebuild personal configs";
      script =
        let
          nix = config.nix.package;
          flakeRef = "git+file:///home/pi/.config/nixpkgs";
          updatesFlakeRef = "github:adomixaszvers/dotfiles-nix/update_flake_lock_action";
        in
        # bash
        ''
          PATH=${lib.makeBinPath [ nix ]}:$PATH
          nix flake archive --refresh '${updatesFlakeRef}'
          nix build --no-link --keep-going --inputs-from '${updatesFlakeRef}' '${flakeRef}#nixosConfigurations.raspberrypi-nixos.config.system.build.toplevel' '${flakeRef}#homeConfigurations.pi.activationPackage' '${flakeRef}#devShells.aarch64-linux.default'
        '';
      startAt = "Fri, 06:00";
    };
  };
}

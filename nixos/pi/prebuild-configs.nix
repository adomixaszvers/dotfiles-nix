{ config, lib, ... }: {
  systemd = {
    services.prebuild-configs = {
      description = "Prebuild personal configs";
      script = let
        nix = config.nix.package;
        flakeRef = "github:adomixaszvers/dotfiles-nix/update_flake_lock_action";
      in ''
        PATH=${lib.makeBinPath [ nix ]}:$PATH
        nix build --no-link '${flakeRef}#nixosConfigurations.raspberrypi-nixos.config.system.build.toplevel' '${flakeRef}#homeConfigurations.pi.activationPackage'
      '';
      startAt = "Fri, 06:00";
    };
  };
}

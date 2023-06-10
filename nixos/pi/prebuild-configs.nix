{ config, lib, pkgs, ... }: {
  systemd = {
    services.prebuild-configs = {
      serviceConfig.User = "pi";
      description = "Prebuild personal configs";
      script = let
        nix = config.nix.package;
        flakeRef = "github:adomixaszvers/dotfiles-nix/update_flake_lock_action";
      in ''
        PATH=${lib.makeBinPath [ nix pkgs.git ]}:$PATH
        nix flake archive --refresh '${flakeRef}'
        nix build --no-link --inputs-from '${flakeRef}' --keep-going 'mine#nixosConfigurations.raspberrypi-nixos.config.system.build.toplevel' 'mine#homeConfigurations.pi.activationPackage' 'mine#devShells.aarch64-linux.default'
      '';
      startAt = "Fri, 06:00";
    };
  };
}

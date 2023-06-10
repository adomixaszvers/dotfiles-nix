{ config, lib, pkgs, ... }: {
  systemd = {
    services.prebuild-configs = {
      serviceConfig.User = "adomas";
      description = "Prebuild personal configs";
      script = let
        nix = config.nix.package;
        flakeRef = "github:adomixaszvers/dotfiles-nix/update_flake_lock_action";
      in ''
        PATH=${lib.makeBinPath [ nix pkgs.git ]}:$PATH
        nix flake archive --refresh '${flakeRef}'
        nix build --no-link --inputs-from '${flakeRef}' --keep-going 'mine#nixosConfigurations.adomas-jatuzis-nixos.config.system.build.toplevel' 'mine#homeConfigurations.work.activationPackage' 'mine#homeConfigurations.work-remote.activationPackage' 'mine#devShells.x86_64-linux.default'
      '';
      startAt = "Fri, 06:00";
    };
  };
}

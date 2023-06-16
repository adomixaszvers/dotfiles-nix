{ config, lib, ... }: {
  systemd = {
    services.prebuild-configs = {
      description = "Prebuild personal configs";
      script = let
        nix = config.nix.package;
        flakeRef = "github:adomixaszvers/dotfiles-nix/update_flake_lock_action";
      in ''
        PATH=${lib.makeBinPath [ nix ]}:$PATH
        nix flake archive --refresh '${flakeRef}'
        nix build --no-link '${flakeRef}#nixosConfigurations.adomas-jatuzis-nixos.config.system.build.toplevel' '${flakeRef}#homeConfigurations.work.activationPackage' '${flakeRef}#homeConfigurations.work-remote.activationPackage' '${flakeRef}#devShells.x86_64-linux.default'
      '';
      startAt = "Fri, 06:00";
    };
  };
}

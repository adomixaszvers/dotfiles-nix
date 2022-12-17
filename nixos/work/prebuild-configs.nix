{ config, lib, ... }: {
  systemd = {
    services.prebuild-configs = {
      description = "Prebuild personal configs";
      script = let
        nix = config.nix.package;
        flakeRef = "github:adomixaszvers/dotfiles-nix/update_flake_lock_action";
      in ''
        PATH=${lib.makeBinPath [ nix ]}:$PATH
        nix build --no-link '${flakeRef}#nixosConfigurations.adomas-jatuzis-nixos.config.system.build.toplevel' '${flakeRef}#homeConfigurations.work.activationPackage' '${flakeRef}#homeConfigurations.work-remote.activationPackage'
      '';
      startAt = "Fri, 06:00";
    };
  };
}

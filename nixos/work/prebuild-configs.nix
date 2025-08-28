{ config, lib, ... }:
{
  systemd = {
    services.prebuild-configs = {
      serviceConfig.User = "adomas";
      description = "Prebuild personal configs";
      script =
        let
          nix = config.nix.package;
          flakeRef = "git+file:///home/adomas/.config/nixpkgs";
          referenced = "github:adomixaszvers/dotfiles-nix/update_flake_lock_action";
        in
        # bash
        ''
          PATH=${lib.makeBinPath [ nix ]}:$PATH
          nix flake archive --refresh '${referenced}'
          nix build --no-link --no-write-lock-file --reference-lock-file '${referenced}' '${flakeRef}#nixosConfigurations.adomas-jatuzis-nixos.config.system.build.toplevel' '${flakeRef}#homeConfigurations.work.activationPackage' '${flakeRef}#homeConfigurations.work-remote.activationPackage' '${flakeRef}#devShells.x86_64-linux.default'
        '';
      startAt = "Fri, 06:00";
    };
  };
}

{ pkgs, ... }:
{
  systemd = {
    services.prebuild-configs = {
      path = [
        pkgs.nix-fast-build
      ];
      serviceConfig.User = "adomas";
      description = "Prebuild personal configs";
      script =
        let
          flakeRef = "github:adomixaszvers/dotfiles-nix/update_flake_lock_action";
        in
        # bash
        ''
          nix-fast-build --skip-cached --no-nom --no-link --max-jobs 4 --eval-workers 4 --cachix-cache adomixaszvers --flake '${flakeRef}#legacyPackages'
        '';
      startAt = "Fri, 06:00";
    };
  };
}

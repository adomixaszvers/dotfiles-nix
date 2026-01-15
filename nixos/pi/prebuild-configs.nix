{ pkgs, ... }:
{
  systemd = {
    services.prebuild-configs = {
      path = [
        pkgs.nix-fast-build
      ];
      serviceConfig.User = "pi";
      description = "Prebuild personal configs";
      script =
        let
          flakeRef = "github:adomixaszvers/dotfiles-nix/update_flake_lock_action";
        in
        # bash
        ''
          nix-fast-build --skip-cached --no-nom --no-link --max-jobs 2 --eval-workers 1 --cachix-cache adomixaszvers --flake '${flakeRef}#legacyPackages'
        '';
      startAt = "Fri, 06:00";
    };
  };
}

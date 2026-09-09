{ config, pkgs, ... }:
{
  systemd.user.timers."repack-nix-tarball-cache" = {
    Install = {
      WantedBy = [ "timers.target" ];
    };
    Timer = {
      OnCalendar = "daily";
      Unit = "repack-nix-tarball-cache.service";
    };
  };

  systemd.user.services."repack-nix-tarball-cache" = {
    Unit.Description = "Repack nix's tarball cache";

    Service = {
      Type = "oneshot";
      ExecStart = pkgs.writeShellScript "repack-nix-tarball-cache" ''
        set -eu
        cd ${config.xdg.cacheHome}/nix/tarball-cache-v2
        ${pkgs.git}/bin/git multi-pack-index write
        ${pkgs.git}/bin/git multi-pack-index repack --batch-size 1024m
        ${pkgs.git}/bin/git multi-pack-index expire
      '';
    };
  };
}

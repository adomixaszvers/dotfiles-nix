{ inputs, pkgs, lib, config, ... }: {
  imports = [ ./cli ./gui ./wm/common.nix ];
  manual.html.enable = true;
  nix.package = lib.mkDefault pkgs.nix;
  programs.home-manager = {
    enable = true;
    path = inputs.home-manager.outPath;
  };
  systemd.user = {
    startServices = "sd-switch";
    services.expire-hm-profiles = {
      Unit.Description = "Clean up HM user profiles.";

      Service = {
        Type = "oneshot";
        ExecStart = lib.getExe (pkgs.writeShellApplication {
          name = "expire-hm-profiles-start-script";
          text = ''
            ${lib.getExe config.nix.package} profile wipe-history \
              --profile "${config.xdg.stateHome}/nix/profiles/home-manager" \
              --older-than '14d'
          '';
        });
      };
    };
    timers.expire-hm-profiles = {
      Unit.Description = "Clean up HM user profiles.";

      Timer = {
        OnCalendar = "weekly";
        Persistent = true;
      };

      Install.WantedBy = [ "timers.target" ];
    };
  };

  xdg.enable = true;
}

{ config, pkgs, lib, ... }:
let inherit (config.services) kbdd;
in with lib; {
  options.services.kbdd.enable = mkEnableOption "Start kbdd";
  config = mkIf kbdd.enable {
    systemd.user.services.kbdd = {
      Unit = {
        Description = "XKB daemon";
        After = [ "graphical-session-pre.target" ];
        PartOf = [ "graphical-session.target" ];
      };

      Install = { WantedBy = [ "graphical-session.target" ]; };

      Service = {
        ExecStart = "${pkgs.kbdd}/bin/kbdd --nodaemon";
        Restart = "always";
        RestartSec = 3;
      };
    };
  };
}

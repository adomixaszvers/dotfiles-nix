{ pkgs, lib, ... }:
let
  swaylock = lib.getExe pkgs.swaylock-effects;
in
{
  programs.swaylock = {
    enable = true;
    package = swaylock;
  };
  services.swayidle = {
    enable = true;
    events = [
      {
        event = "before-sleep";
        command = "${pkgs.systemd}/bin/loginctl lock-session";
      }
      {
        event = "lock";
        command = "${swaylock} -f";
      }
    ];
    timeouts = [
      {
        timeout = 300;
        command = "${swaylock} -f";
      }
    ];
  };
  stylix.targets.swaylock.enable = true;
}

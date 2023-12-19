{ config, pkgs, ... }:
let swaylock = pkgs.swaylock-effects;
in {
  programs.swaylock = {
    enable = true;
    package = swaylock;
    settings.color = builtins.substring 1 6 config.colors.background;
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
        command = "${swaylock}/bin/swaylock";
      }
    ];
    timeouts = [{
      timeout = 300;
      command = "${swaylock}/bin/swaylock";
    }];
  };
}

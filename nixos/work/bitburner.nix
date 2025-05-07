{ pkgs, ... }:
let
  inherit (pkgs) xpra;
in
{
  environment.systemPackages = [ xpra ];
  users = {
    users.bitburner = {
      isSystemUser = true;
      group = "bitburner";
      home = "/var/lib/bitburner";
      createHome = true;
      description = "BitBurner daemon user";
    };
    groups.bitburner = { };
  };
  systemd.services.bitburner = {
    path = [
      pkgs.chromium
      xpra
    ];
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    script = ''
      xpra start :99 --bind-tcp=127.0.0.1:9292 --daemon=no --notifications=no --printing=no --speaker=no --session-name=bitburner --exit-with-children --start-child="chromium --app=https://bitburner-official.github.io/ --user-data-dir=/var/lib/bitburner/.bitburner-profile"
    '';
    serviceConfig = {
      User = "bitburner";
      RestartSec = 60;
      Restart = "always";
      MemoryHigh = "2G";
      MemoryMax = "4G";
    };
  };
}

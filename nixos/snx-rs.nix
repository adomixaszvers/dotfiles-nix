{ pkgs, ... }:
{
  environment.systemPackages = [ pkgs.snx-rs ];
  systemd.services.snx-rs = {
    description = "SNX-RS Service";
    wantedBy = [ "multi-user.target" ];
    after = [ "network.target" ];
    path = [
      pkgs.iproute2
      pkgs.kmod
    ];
    serviceConfig = {
      Type = "simple";
      ExecStart = "${pkgs.snx-rs}/bin/snx-rs -m command";
      User = "root";
      Restart = "on-failure";
    };
  };
}

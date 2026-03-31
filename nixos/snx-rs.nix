{ pkgs, ... }:
{
  environment.systemPackages = [ pkgs.snx-rs ];
  systemd.services.snx-rs = {
    description = "SNX-RS VPN client for Linux";
    wantedBy = [ "multi-user.target" ];
    after = [ "network-online.target" ];
    wants = [ "network-online.target" ];
    path = [
      pkgs.iproute2
      pkgs.kmod
      pkgs.networkmanager
    ]; # for ip, modprobe and nmcli commands

    serviceConfig = {
      Type = "simple";
      ExecStart = "${pkgs.snx-rs}/bin/snx-rs -m command -l debug";
      Restart = "on-failure";
    };
  };

  # update the firewall rule to allow keepalive traffic
  networking.firewall.checkReversePath = "loose";
}

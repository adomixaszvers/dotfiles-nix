{ pkgs, ... }: {
  environment.systemPackages = with pkgs; [ iperf3 ];
  networking.firewall.allowedTCPPorts = [ 5201 ];
}

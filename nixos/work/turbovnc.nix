{ pkgs, ... }:
{
  environment.systemPackages = [ pkgs.turbovnc ];
  networking.firewall.interfaces.wg0.allowedTCPPortRanges = [
    {
      from = 5800;
      to = 5999;
    }
  ];
}

{ config, ... }:
{
  networking.hosts."127.0.0.1" = builtins.attrNames config.services.nginx.virtualHosts;
  networking.firewall.allowedTCPPorts = [
    80
    443
  ];
  services.nginx = {
    enable = true;
    group = "acme";
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
  };
}

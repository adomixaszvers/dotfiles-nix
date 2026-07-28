{
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

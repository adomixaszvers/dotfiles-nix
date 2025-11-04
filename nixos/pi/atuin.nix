let
  port = 8090;
in
{
  services = {
    atuin = {
      inherit port;
      enable = true;
      database.createLocally = true;
      openRegistration = true;
      host = "127.0.0.1";
    };
    nginx.virtualHosts = {
      "atuin.rpi4.beastade.top" = {
        useACMEHost = "rpi4.beastade.top";
        locations = {
          "/" = {
            proxyPass = "http://127.0.0.1:${toString port}";
          };
        };
        forceSSL = true;
      };
    };
  };
  networking.firewall.interfaces = {
    eth0.allowedTCPPorts = [ port ];
    wg0.allowedTCPPorts = [ port ];
  };
}

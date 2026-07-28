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
      "atuin.bl.beastade.top" = {
        useACMEHost = "bl.beastade.top";
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
    enp2s0.allowedTCPPorts = [ port ];
    wg0.allowedTCPPorts = [ port ];
  };
}

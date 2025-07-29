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
    nginx.virtualHosts =
      let
        locations = {
          "/" = {
            proxyPass = "http://127.0.0.1:${toString port}";
          };
        };
        forceSSL = true;
      in
      {
        "atuin.lan.beastade.top" = {
          useACMEHost = "lan.beastade.top";
          inherit forceSSL locations;
        };
        "atuin.wg.beastade.top" = {
          useACMEHost = "wg.beastade.top";
          inherit forceSSL locations;
        };
      };
  };
  networking.firewall.interfaces = {
    eth0.allowedTCPPorts = [ port ];
    wg0.allowedTCPPorts = [ port ];
  };
}

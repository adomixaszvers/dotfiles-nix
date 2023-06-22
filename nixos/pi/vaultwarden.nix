{
  services.vaultwarden = {
    enable = true;
    config = {
      signupsAllowed = true;
      rocketPort = 8222;
      rocketLog = "critical";
    };
  };
  services.nginx.virtualHosts = let
    locations = { "/" = { proxyPass = "http://127.0.0.1:8222"; }; };
    forceSSL = true;
  in {
    "vaultwarden.lan.beastade.top" = {
      useACMEHost = "lan.beastade.top";
      inherit forceSSL locations;
    };
    "vaultwarden.wg.beastade.top" = {
      useACMEHost = "wg.beastade.top";
      inherit forceSSL locations;
    };
  };
}

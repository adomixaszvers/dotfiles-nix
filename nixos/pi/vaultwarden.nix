{ config, ... }:
{
  sops.secrets."vaultwarden.env" = {
    sopsFile = ./secrets/vaultwarden.env;
    owner = config.users.users.vaultwarden.name;
    format = "dotenv";
    inherit (config.users.users.vaultwarden) group;
  };
  services.vaultwarden = {
    enable = true;
    config = {
      signupsAllowed = true;
      rocketPort = 8222;
      rocketLog = "critical";
    };
    environmentFile = config.sops.secrets."vaultwarden.env".path;
  };
  services.nginx.virtualHosts = {
    "vaultwarden.rpi4.beastade.top" = {
      useACMEHost = "rpi4.beastade.top";
      locations =
        let
          proxyPass = "http://127.0.0.1:8222";
        in
        {
          "/" = {
            inherit proxyPass;
          };
          "/notifications/hub" = {
            inherit proxyPass;
            proxyWebsockets = true;
          };
        };
      forceSSL = true;
    };
  };
}

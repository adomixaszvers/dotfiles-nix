{ config, ... }: {
  sops.secrets."vaultwarden.env" = {
    sopsFile = ./secrets/vaulwarden.env;
    owrner = config.users.users.vaultwarden.name;
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

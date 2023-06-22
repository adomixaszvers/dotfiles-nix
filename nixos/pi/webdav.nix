{ config, ... }: {
  sops.secrets.webdav = {
    sopsFile = ./secrets/webdav.env;
    format = "binary";
  };
  services.nginx.virtualHosts = let
    locations = { "/" = { proxyPass = "http://127.0.0.1:8080"; }; };
    forceSSL = true;
  in {
    "webdav.lan.beastade.top" = {
      useACMEHost = "lan.beastade.top";
      inherit forceSSL locations;
    };
    "webdav.wg.beastade.top" = {
      useACMEHost = "wg.beastade.top";
      inherit forceSSL locations;
    };
  };
  services.webdav = {
    enable = true;
    user = "syncthing";
    environmentFile = config.sops.secrets.webdav.path;
    settings = {
      address = "127.0.0.1";
      port = 8080;
      scope = "/var/lib/syncthing/keepass";
      modify = true;
      auth = true;
      users = [{
        username = "{env}USER";
        password = "{env}PASSWORD";
      }];
    };
  };
}

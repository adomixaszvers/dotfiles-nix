{ config, ... }:
{
  sops.secrets.webdav = {
    sopsFile = ./secrets/webdav.env;
    format = "binary";
  };
  services.nginx.virtualHosts = {
    "webdav.rpi4.beastade.top" = {
      useACMEHost = "rpi4.beastade.top";
      locations = {
        "/" = {
          proxyPass = "http://127.0.0.1:8080";
        };
      };
      forceSSL = true;
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
      users = [
        {
          username = "{env}USER";
          password = "{env}PASSWORD";
        }
      ];
    };
  };
}

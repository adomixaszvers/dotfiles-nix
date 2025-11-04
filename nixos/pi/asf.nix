{
  services.archisteamfarm = {
    enable = true;
    web-ui.enable = true;
    settings.CurrentCulture = "en-GB";
    ipcSettings = {
      Kestrel = {
        Endpoints = {
          example-http4.Url = "http://127.0.0.1:1242";
        };
        KnownNetworks = [
          "192.168.1.0/24"
          "10.6.0.0/24"
        ];
      };
    };
  };
  services.nginx.virtualHosts = {
    "asf.rpi4.beastade.top" = {
      useACMEHost = "rpi4.beastade.top";
      locations = {
        "/" = {
          proxyPass = "http://127.0.0.1:1242";
        };
      };
      forceSSL = true;
    };
  };
}

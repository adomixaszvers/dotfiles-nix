{
  services.traefik.dynamicConfigOptions.http = {
    middlewares.vaultwarden-https-redirect.redirectScheme.scheme = "https";
    routers =
      let rule = "HostRegexp(`vaultwarden.{net:(lan|wg|zt)}.beastade.top`)";
      in {
        vaultwarden = {
          inherit rule;
          entrypoints = "web";
          middlewares = "vaultwarden-https-redirect";
          service = "vaultwarden";
        };
        vaultwarden-secure = {
          inherit rule;
          entrypoints = "websecure";
          service = "vaultwarden";
          tls = { };
        };
      };
    services.vaultwarden.loadBalancer.servers =
      [{ url = "http://127.0.0.1:8222"; }];
  };
  services.vaultwarden = {
    enable = true;
    config = {
      signupsAllowed = true;
      rocketPort = 8222;
      rocketLog = "critical";
    };
  };
}

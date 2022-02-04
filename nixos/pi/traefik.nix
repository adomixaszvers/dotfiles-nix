{
  networking.firewall.allowedTCPPorts = [ 80 8080 443 ];
  systemd.services.traefik.environment = {
    NAMESILO_PROPAGATION_TIMEOUT = "3600";
    NAMESILO_POLLING_INTERVAL = "120";
    NAMESILO_TTL = "3600";
  };
  systemd.services.traefik.serviceConfig.EnvironmentFile =
    "/var/lib/traefik/environment";
  services.traefik = {
    enable = true;
    staticConfigOptions = {
      pilot.dashboard = false;
      api = { };
      entrypoints = {
        web.address = ":80";
        websecure.address = ":443";
      };
      certificatesResolvers.namesilo.acme = {
        email = "adomixaszvers@gmail.com";
        storage = "/var/lib/traefik/acme.json";
        dnsChallenge.provider = "namesilo";
      };
    };
    dynamicConfigOptions = {
      http = {
        middlewares = {
          traefik-https-redirect.redirectScheme = { scheme = "https"; };
          sslheader.headers.customRequestHeaders."X-Forwarded-Proto" = "https";
        };
        routers = let
          rule = 
            "HostRegexp(`traefik.{net:(lan|wg|zt)}.beastade.top`)";
        in {
          traefik = {
            inherit rule;
            entrypoints = "web";
            middlewares = "traefik-https-redirect";
            service = "api@internal";
          };
          traefik-secure = {
            inherit rule;
            entrypoints = "websecure";
            service = "api@internal";
            tls = {
              certResolver = "namesilo";
              domains = [
                {
                  main = "lan.beastade.top";
                  sans = [ "*.lan.beastade.top" ];
                }
                {
                  main = "wg.beastade.top";
                  sans = [ "*.wg.beastade.top" ];
                }
                {
                  main = "zt.beastade.top";
                  sans = [ "*.zt.beastade.top" ];
                }
              ];
            };
          };
        };
      };
    };
  };
}

{
  networking.firewall.allowedTCPPorts = [ 8384 ];
  # see https://github.com/lucas-clemente/quic-go/wiki/UDP-Receive-Buffer-Size#non-bsd
  boot.kernel.sysctl."net.core.rmem_max" = 2500000;
  services.syncthing = {
    enable = true;
    user = "pi";
    group = "users";
    dataDir = "/home/pi";
    openDefaultPorts = true;
    guiAddress = "0.0.0.0:8384";
  };
  services.traefik.dynamicConfigOptions.http = {
    middlewares.syncthing-https-redirect.redirectScheme.scheme = "https";
    routers =
      let rule = "HostRegexp(`syncthing.{net:(lan|wg|zt)}.beastade.top`)";
      in {
        syncthing = {
          inherit rule;
          entrypoints = "web";
          middlewares = "syncthing-https-redirect";
          service = "syncthing";
        };
        syncthing-secure = {
          inherit rule;
          entrypoints = "websecure";
          service = "syncthing";
          tls = { };
        };
      };
    services.syncthing.loadBalancer.servers =
      [{ url = "http://127.0.0.1:8384"; }];
  };
}

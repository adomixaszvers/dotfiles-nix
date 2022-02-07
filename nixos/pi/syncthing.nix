{
  # see https://github.com/lucas-clemente/quic-go/wiki/UDP-Receive-Buffer-Size#non-bsd
  boot.kernel.sysctl."net.core.rmem_max" = 2500000;
  services.syncthing = {
    enable = true;
    openDefaultPorts = true;
    guiAddress = "0.0.0.0:8384";
  };
  services.nginx.virtualHosts = let
    locations = { "/" = { proxyPass = "http://127.0.0.1:8384"; }; };
    forceSSL = true;
  in {
    "syncthing.lan.beastade.top" = {
      useACMEHost = "lan.beastade.top";
      inherit forceSSL locations;
    };
    "syncthing.wg.beastade.top" = {
      useACMEHost = "wg.beastade.top";
      inherit forceSSL locations;
    };
    "syncthing.zt.beastade.top" = {
      useACMEHost = "zt.beastade.top";
      inherit forceSSL locations;
    };
  };
}

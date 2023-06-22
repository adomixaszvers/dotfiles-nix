{
  # see https://github.com/lucas-clemente/quic-go/wiki/UDP-Receive-Buffer-Size#non-bsd
  boot.kernel.sysctl."net.core.rmem_max" = 2500000;
  networking.firewall.allowedTCPPorts = [ # 8384
    22000
  ];
  networking.firewall.allowedUDPPorts = [ 22000 21027 ];
  services.syncthing = {
    enable = true;
    openDefaultPorts = true;
    guiAddress = "0.0.0.0:8384";
  };
  services.nginx.virtualHosts = let
    locations = {
      "/" = {
        proxyPass = "http://127.0.0.1:8384";
        # see https://docs.syncthing.net/users/reverseproxy.html#nginx
        extraConfig = ''
          proxy_set_header        X-Real-IP $remote_addr;
          proxy_set_header        X-Forwarded-For $proxy_add_x_forwarded_for;
          proxy_set_header        X-Forwarded-Proto $scheme;
          proxy_read_timeout      600s;
          proxy_send_timeout      600s;
          proxy_headers_hash_max_size 512;
          proxy_headers_hash_bucket_size 128;
        '';
      };
    };
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
  };
}

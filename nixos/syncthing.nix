{
  # see https://github.com/lucas-clemente/quic-go/wiki/UDP-Receive-Buffer-Size#non-bsd
  boot.kernel.sysctl."net.core.rmem_max" = 2500000;
  # Syncthing ports
  networking.firewall.allowedTCPPorts = [ # 8384
    22000
  ];
  networking.firewall.allowedUDPPorts = [ 22000 21027 ];
  services.syncthing = {
    enable = true;
    user = "adomas";
    group = "users";
    dataDir = "/home/adomas";
  };
}

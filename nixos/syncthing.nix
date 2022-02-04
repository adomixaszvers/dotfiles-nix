{
  # see https://github.com/lucas-clemente/quic-go/wiki/UDP-Receive-Buffer-Size#non-bsd
  boot.kernel.sysctl."net.core.rmem_max" = 2500000;
  networking.firewall.allowedTCPPorts = [ 22000 ]; # syncthing
  services.syncthing = {
    enable = true;
    user = "adomas";
    group = "users";
    dataDir = "/home/adomas";
  };
}

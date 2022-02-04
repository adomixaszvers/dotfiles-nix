{ config, ... }: {
  networking.domain = "lan";
  networking.nameservers = [ "127.0.0.1" "1.1.1.1" ];
  networking.firewall = {
    allowedTCPPorts = [ 53 5080 5335 22000 ];
    allowedUDPPorts = [ 53 5335 ];
  };
  networking.resolvconf.useLocalResolver = false;
  services.unbound = {
    enable = true;
    settings = {
      server = {
        # verbosity = 2;
        interface = [ "0.0.0.0@5335" ];
        access-control = "192.168.20.0/24 allow";
        do-ip4 = "yes";
        do-udp = "yes";
        do-tcp = "yes";
        do-ip6 = "no";

        prefer-ip6 = "no";

        # Trust glue only if it is within the server's authority
        harden-glue = "yes";
        # Require DNSSEC data for trust-anchored zones, if such data is absent, the zone become    s BOGUS
        harden-dnssec-stripped = "yes";
        # Don't use Capitalization randomization as it known to cause DNSSEC issues sometimes
        # see https://discourse.pi-hole.net/t/unbound-stubby-or-dnscrypt-proxy/9378 for further details
        use-caps-for-id = "no";
        # Reduce EDNS reassembly buffer size.
        # Suggested by the unbound man page to reduce fragmentation reassembly problems
        edns-buffer-size = 1472;
        # Perform prefetching of close to expired message cache entries
        # This only applies to domains that have been frequently queried
        prefetch = "yes";
        # Ensure kernel buffer is large enough to not lose messages in traffic spikes
        so-rcvbuf = "1m";
        # Ensure privacy of local IP ranges
        private-address = [
          "192.168.0.0/16"
          "169.254.0.0/16"
          "172.16.0.0/12"
          "10.0.0.0/8"
          "fd00::/8"
          "fe80::/10"
        ];

        local-zone = [
          "'lan.beastade.top' redirect"
          "'wg.beastade.top' redirect"
          "'zt.beastade.top' redirect"
        ];
        local-data = [
          "'lan.beastade.top A 192.168.1.207'"
          "'wg.beastade.top A 10.6.0.1'"
          "'zt.beastade.top A 10.147.17.157'"
        ];

        # Send minimum amount of information to upstream servers to enhance
        # privacy. Only sends minimum required labels of the QNAME and sets
        # QTYPE to NS when possible.
        # See RFC 7816 "DNS Query Name Minimisation to Improve Privacy" for
        # details.
        qname-minimisation = "yes";

        private-domain = "lan";
        domain-insecure = "lan";
        unblock-lan-zones = "yes";
        insecure-lan-zones = "yes";
      };
      forward-zone = [
        {
          name = "lan";
          forward-addr = "192.168.1.254";
        }
        {
          name = "1.168.192.in-addr.arpa";
          forward-addr = "192.168.1.254";
        }
      ];
    };
  };
  services.traefik.dynamicConfigOptions.http = {
    middlewares.pihole-https-redirect.redirectScheme.scheme = "https";
    routers = let rule = "HostRegexp(`pihole.{net:(lan|wg|zt)}.beastade.top`)";
    in {
      pihole = {
        inherit rule;
        entrypoints = "web";
        middlewares = "pihole-https-redirect";
        service = "pihole-admin";
      };
      pihole-secure = {
        inherit rule;
        entrypoints = "websecure";
        service = "pihole-admin";
        tls = { };
      };
    };
    services.pihole-admin.loadBalancer.servers =
      [{ url = "http://127.0.0.1:5080"; }];
  };

  sops.secrets."pihole/environment" = { sopsFile = ./secrets/pihole.yaml; };
  virtualisation.oci-containers.containers.pihole = {
    autoStart = true;
    image = "pihole/pihole:latest";
    environment = {
      TZ = "Europe/Vilnius";
      "PIHOLE_DNS_" = "192.168.20.1#5335";
    };
    environmentFiles = [ config.sops.secrets."pihole/environment".path ];
    ports = [ "5080:80" "53:53" "53:53/udp" ];
    extraOptions = [ "--network=proxy" ];
    volumes = [
      "/home/pi/pihole/etc-pihole/:/etc/pihole/"
      "/home/pi/pihole/etc-dnsmasq.d/:/etc/dnsmasq.d/"
    ];
  };
}

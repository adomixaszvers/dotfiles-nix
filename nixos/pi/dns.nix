{ config, pkgs, ... }: {
  networking = {
    domain = "lan";
    nameservers = [ "127.0.0.1" "9.9.9.9" ];
    firewall = {
      allowedTCPPorts = [ 53 5080 ];
      allowedUDPPorts = [ 53 ];
    };
  };
  services.unbound = {
    enable = true;
    settings = {
      server = {
        # verbosity = 2;
        interface = [ "127.0.0.1" "192.168.20.1" ];
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

        local-zone =
          [ "'lan.beastade.top' redirect" "'wg.beastade.top' redirect" ];
        local-data = [
          "'lan.beastade.top A 192.168.1.207'"
          "'wg.beastade.top A 10.6.0.1'"
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
  services.nginx.virtualHosts = let
    locations = { "/" = { proxyPass = "http://192.168.20.2:80"; }; };
    forceSSL = true;
  in {
    "pihole.lan.beastade.top" = {
      useACMEHost = "lan.beastade.top";
      inherit forceSSL locations;
    };
    "pihole.wg.beastade.top" = {
      useACMEHost = "wg.beastade.top";
      inherit forceSSL locations;
    };
  };

  users = {
    users.pihole = {
      isSystemUser = true;
      group = "pihole";
      home = "/var/lib/pihole";
      createHome = true;
      description = "Pihole daemon user";
      uid = 995;
    };
    groups.pihole = { gid = 992; };
  };

  systemd.services = let name = "proxy";
  in {
    "podman-network-${name}" = rec {
      wantedBy = [ "multi-user.target" ];
      after = [ "podman.service" "podman.socket" ];
      before = [ "podman-pihole.service" ];
      requires = after;
      serviceConfig = {
        ExecStart = pkgs.writeShellScript "podman-network-create-${name}" ''
          if [ -z "$(${pkgs.podman}/bin/podman network ls -q | grep ${name})" ]; then
             ${pkgs.podman}/bin/podman network create --disable-dns --gateway=192.168.20.1 --subnet=192.168.20.0/24 ${name}
          fi
        '';
        ExecStop = ''
          ${pkgs.podman}/bin/podman network rm ${name}
        '';
        RemainAfterExit = "true";
        Type = "oneshot";
      };
    };
  };

  sops.secrets."pihole/environment" = { sopsFile = ./secrets/pihole.yaml; };
  virtualisation.oci-containers.containers.pihole = {
    autoStart = true;
    image = "pihole/pihole:latest";
    # user = "pihole:pihole";
    environment = {
      TZ = "Europe/Vilnius";
      "PIHOLE_DNS_" = "192.168.20.1";
      FTLCONF_REPLY_ADDR4 = "192.168.1.207";
      DNSMASQ_USER = "pihole";
      PIHOLE_UID = "995";
      PIHOLE_GID = "992";
      WEB_UID = "995";
      WEB_GID = "992";
      FTLCONF_BLOCK_TTL = "2400";
    };
    environmentFiles = [ config.sops.secrets."pihole/environment".path ];
    ports = [
      "10.6.0.1:53:53"
      "10.6.0.1:53:53/udp"
      "192.168.1.207:53:53"
      "192.168.1.207:53:53/udp"
      "192.168.1.207:5080:80"
    ];
    extraOptions = [ "--network=proxy" "--ip=192.168.20.2" ];
    volumes = [
      "/var/lib/pihole/etc-pihole/:/etc/pihole/"
      "/var/lib/pihole/etc-dnsmasq.d/:/etc/dnsmasq.d/"
    ];
  };
}

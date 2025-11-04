{ config, pkgs, ... }:
{
  networking = {
    domain = "lan";
    nameservers = [ "192.168.1.254" ];
    firewall = {
      allowedTCPPorts = [
        53
        5080
      ];
      allowedUDPPorts = [ 53 ];
    };
  };
  services.nginx.virtualHosts = {
    "pihole.rpi4.beastade.top" = {
      useACMEHost = "rpi4.beastade.top";
      locations = {
        "/" = {
          proxyPass = "http://192.168.1.207:5080";
        };
      };
      forceSSL = true;
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
    groups.pihole = {
      gid = 992;
    };
  };

  sops.secrets."pihole/environment" = {
    sopsFile = ./secrets/pihole.yaml;
  };
  virtualisation.oci-containers.containers.pihole = {
    autoStart = true;
    image = "pihole/pihole:latest";
    # user = "pihole:pihole";
    environment = {
      TZ = "Europe/Vilnius";
      "PIHOLE_DNS_" = "9.9.9.9;149.112.112.112";
      FTLCONF_LOCAL_ADDR4 = "192.168.1.207";
      DNSMASQ_USER = "pihole";
      PIHOLE_UID = "995";
      PIHOLE_GID = "992";
      WEB_UID = "995";
      WEB_GID = "992";
      FTLCONF_BLOCK_TTL = "2400";
      IPv6 = "false";
      FTLCONF_RATE_LIMIT = "0/0"; # disable rate limiting
    };
    environmentFiles = [ config.sops.secrets."pihole/environment".path ];
    ports = [
      "10.6.0.1:53:53"
      "10.6.0.1:53:53/udp"
      "192.168.1.207:53:53"
      "192.168.1.207:53:53/udp"
      "192.168.1.207:5080:80"
    ];
    volumes = [
      "/var/lib/pihole/etc-pihole/:/etc/pihole/"
      "/var/lib/pihole/etc-dnsmasq.d/:/etc/dnsmasq.d/"
      "${pkgs.writeText "42-reverse-proxied-subdomains.conf" ''
        address=/wg.beastade.top/10.6.0.1
        address=/lan.beastade.top/192.168.1.207
        rev-server=192.168.1.0/24,192.168.1.254
        local=/lan/192.168.1.254
        local-ttl=3600
      ''}:/etc/dnsmasq.d/42-reverse-proxied-subdomains.conf:ro"
    ];
  };
}

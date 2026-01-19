{ pkgs, config, ... }:
{
  sops.secrets = {
    "wireguard/privateKey".sopsFile = ./secrets/wireguard.yaml;
    "wireguard/presharedKeys/telefonas".sopsFile = ../common-secrets/wireguard.yaml;
    "wireguard/presharedKeys/rutosKompas".sopsFile = ../common-secrets/wireguard.yaml;
    "wireguard/presharedKeys/pc".sopsFile = ../common-secrets/wireguard/pc.yaml;
    "wireguard/presharedKeys/work".sopsFile = ../common-secrets/wireguard/work.yaml;
    "wireguard/presharedKeys/laptop".sopsFile = ../common-secrets/wireguard/laptop.yaml;
    "wireguard/presharedKeys/samsung".sopsFile = ../common-secrets/wireguard.yaml;
    "wireguard/presharedKeys/rycioKompas".sopsFile = ../common-secrets/wireguard.yaml;
    "wireguard/presharedKeys/t14".sopsFile = ../common-secrets/wireguard/t14.yaml;
    "wireguard/presharedKeys/steamdeck".sopsFile = ../common-secrets/wireguard.yaml;
  };
  networking = {
    nat.enable = true;
    firewall.interfaces.eth0 = {
      allowedTCPPorts = [ 51820 ];
      allowedUDPPorts = [ 51820 ];
    };
    wireguard = {
      enable = true;
      interfaces.wg0 = {
        ips = [ "10.6.0.1/24" ];
        listenPort = 51820;
        privateKeyFile = config.sops.secrets."wireguard/privateKey".path;

        # This allows the wireguard server to route your traffic to the internet and hence be like a VPN
        # For this to work you have to set the dnsserver IP of your router (or dnsserver of choice) in your clients
        postSetup = ''
          ${pkgs.iptables}/bin/iptables -t nat -A POSTROUTING -s 10.6.0.0/24 -o eth0 -j MASQUERADE
        '';

        # This undoes the above command
        postShutdown = ''
          ${pkgs.iptables}/bin/iptables -t nat -D POSTROUTING -s 10.6.0.0/24 -o eth0 -j MASQUERADE
        '';
        peers = [
          {
            # adomo-telefonas
            publicKey = "8aQnXeb3mM7JCRYp34CZ7NYPBr/LEhiWBx5icpln014=";
            presharedKeyFile = config.sops.secrets."wireguard/presharedKeys/telefonas".path;
            allowedIPs = [ "10.6.0.2/32" ];
          }
          {
            # rutos-kompas
            publicKey = "lyniEfPL2BMwFS6lrip/smKNjpLPWe/LiyIWtGVQ/H4=";
            presharedKeyFile = config.sops.secrets."wireguard/presharedKeys/rutosKompas".path;
            allowedIPs = [ "10.6.0.3/32" ];
          }
          {
            # windows-new
            publicKey = "eJvrBUU4+8zPRm/k/BgZTzimrZk97pKxz/GvVApBohE=";
            presharedKeyFile = config.sops.secrets."wireguard/presharedKeys/pc".path;
            allowedIPs = [ "10.6.0.5/32" ];
          }
          {
            # work
            publicKey = "C1PNVCRetK41/lxm3/lT5dZRzw+z8p99YNkroIilHEA=";
            presharedKeyFile = config.sops.secrets."wireguard/presharedKeys/work".path;
            allowedIPs = [ "10.6.0.6/32" ];
          }
          {
            # laptop
            publicKey = "tINtePHKYYaoh+H3k9WTozeNcrWay7hZ3mq1HY0I+Fg=";
            presharedKeyFile = config.sops.secrets."wireguard/presharedKeys/laptop".path;
            allowedIPs = [ "10.6.0.7/32" ];
          }
          {
            # samsung
            publicKey = "RRUoaihwS9llZeUYLQUvWc2B+ND25leP6hW+As5Hsw4=";
            presharedKeyFile = config.sops.secrets."wireguard/presharedKeys/samsung".path;
            allowedIPs = [ "10.6.0.10/32" ];
          }
          {
            publicKey = "rnh9/k44jIP57tubQNBcMz35J4JuoP1AN7Az8St8cmk=";
            presharedKeyFile = config.sops.secrets."wireguard/presharedKeys/t14".path;
            allowedIPs = [ "10.6.0.11/32" ];
          }
          {
            publicKey = "GJFNOA0GxnOGoigrw5g5JksQhSAMRRKT6PXE2rZL7xw=";
            presharedKeyFile = config.sops.secrets."wireguard/presharedKeys/rycioKompas".path;
            allowedIPs = [ "10.6.0.12/32" ];
          }
          {
            publicKey = "r0v6/SesQlu9ZubRzeEkKFC3pBQLH4XePaCZojA3oGY=";
            presharedKeyFile = config.sops.secrets."wireguard/presharedKeys/steamdeck".path;
            allowedIPs = [ "10.6.0.13/32" ];
          }
        ];
      };
    };
  };
}

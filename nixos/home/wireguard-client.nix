{ config, pkgs, ... }: {
  sops.secrets."wireguard/presharedKeys/laptop" = {
    sopsFile = ../common-secrets/wireguard.yaml;
  };
  sops.secrets."wireguard/privateKey" = {
    sopsFile = ./secrets/wireguard.yaml;
  };
  networking.firewall.trustedInterfaces = [ "wg0" ];
  networking.wg-quick = {
    interfaces.wg0 = {
      address = [ "10.6.0.7/24" ];
      privateKeyFile = config.sops.secrets."wireguard/privateKey".path;
      peers = [{
        publicKey = "FQV2434Wk2isZNZYDEr8R5yAf11B3K/tL//RIvW1qWw=";
        presharedKeyFile =
          config.sops.secrets."wireguard/presharedKeys/laptop".path;
        endpoint = "192.168.1.207:51820";
        allowedIPs = [ "10.6.0.0/24" ];
        persistentKeepalive = 25;
      }];
    };
  };
}

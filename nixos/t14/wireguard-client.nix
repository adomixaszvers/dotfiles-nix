{ config, ... }: {
  sops.secrets."wireguard/presharedKeys/t14" = {
    sopsFile = ../common-secrets/wireguard.yaml;
  };
  sops.secrets."wireguard/privateKey" = {
    sopsFile = ./secrets/wireguard.yaml;
  };
  networking.firewall.trustedInterfaces = [ "wg0" ];
  networking.wg-quick = {
    interfaces.wg0 = {
      address = [ "10.6.0.11/24" ];
      privateKeyFile = config.sops.secrets."wireguard/privateKey".path;
      peers = [{
        publicKey = "FQV2434Wk2isZNZYDEr8R5yAf11B3K/tL//RIvW1qWw=";
        presharedKeyFile =
          config.sops.secrets."wireguard/presharedKeys/t14".path;
        endpoint = "192.168.1.207:51820";
        allowedIPs = [ "10.6.0.0/24" ];
        persistentKeepalive = 25;
      }];
    };
  };
}

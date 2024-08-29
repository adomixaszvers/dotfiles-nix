{ config, ... }:
{
  sops.secrets = {
    "wireguard/presharedKeys/windowsNew".sopsFile = ../common-secrets/wireguard.yaml;
    "wireguard/privateKey".sopsFile = ./secrets/wireguard.yaml;
  };
  networking.firewall.trustedInterfaces = [ "wg0" ];
  networking.wg-quick = {
    interfaces.wg0 =
      let
        publicKey = "FQV2434Wk2isZNZYDEr8R5yAf11B3K/tL//RIvW1qWw=";
      in
      {
        address = [ "10.6.0.5/24" ];
        privateKeyFile = config.sops.secrets."wireguard/privateKey".path;
        peers = [
          {
            inherit publicKey;
            presharedKeyFile = config.sops.secrets."wireguard/presharedKeys/windowsNew".path;
            endpoint = "192.168.1.207:51820";
            # allowedIPs = [ "0.0.0.0/0" ];
            allowedIPs = [ "10.6.0.0/24" ];
            persistentKeepalive = 25;
          }
        ];
      };
  };
}

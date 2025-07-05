{ config, pkgs, ... }:
{
  environment.etc."NetworkManager/dnsmasq.d/wireguard".text = # ini
    ''
      addn-hosts=/etc/hosts
      address=/wg.beastade.top/10.6.0.1
      address=/lan.beastade.top/10.6.0.1
      server=/wg/10.6.0.1
      rev-server=10.6.0.0/24,10.6.0.1
    '';
  sops.secrets = {
    "wireguard/privateKey".sopsFile = ./secrets/wireguard.yaml;
    "wireguard/endpoint".sopsFile = ./secrets/wireguard.yaml;
    "wireguard/presharedKeys/work".sopsFile = ../common-secrets/wireguard/work.yaml;
  };
  networking = {
    networkmanager.dns = "dnsmasq";
    firewall = {
      trustedInterfaces = [ "wg0" ];
      # interfaces = {
      #   # eno1 = { allowedUDPPorts = [ 51820 ]; };
      #   wg0 = { allowedTCPPorts = [ 1080 3389 5901 22000 8443 5900 3128 ]; };
      # };
    };
    # nameservers = lib.mkBefore [ "10.6.0.1" ];
    wg-quick.interfaces.wg0 =
      let
        publicKey = "FQV2434Wk2isZNZYDEr8R5yAf11B3K/tL//RIvW1qWw=";
      in
      {
        address = [ "10.6.0.6/24" ];
        # listenPort = 51820;
        postUp = ''
          wg set wg0 peer ${publicKey} endpoint $(<${config.sops.secrets."wireguard/endpoint".path})
        '';
        peers = [
          {
            inherit publicKey;
            presharedKeyFile = config.sops.secrets."wireguard/presharedKeys/work".path;
            allowedIPs = [
              "10.6.0.0/24"
              "192.168.1.0/24"
            ];
            persistentKeepalive = 25;
          }
        ];
        privateKeyFile = config.sops.secrets."wireguard/privateKey".path;
      };
  };
  systemd.services.check-wg = {
    description = "Check if WG is still working";
    script = "${pkgs.inetutils}/bin/ping -c5 10.6.0.1 || ${pkgs.systemd}/bin/systemctl restart wg-quick-wg0";
    startAt = "Mon-Fri 01:00";
  };
}

{ config, ... }:
{
  environment.etc."NetworkManager/dnsmasq.d/nginx".text = # ini
    ''
      address=/l15.beastade.top/127.0.0.1
    '';
  sops = {
    templates."acme.env".content = ''
      NAMESILO_API_KEY=${config.sops.placeholder."acme/namesiloApiKey"}
      NAMESILO_PROPAGATION_TIMEOUT=3600
      NAMESILO_POLLING_INTERVAL=120
      NAMESILO_TTL=3600
    '';
    secrets."acme/namesiloApiKey".sopsFile = ../common-secrets/acme.yaml;
  };
  security.acme = {
    acceptTerms = true;
    defaults.email = "adomixaszvers@gmail.com";
    certs =
      let
        dnsProvider = "namesilo";
        dnsResolver = "ns1.dnsowl.com:53";
        credentialsFile = config.sops.templates."acme.env".path;
      in
      {
        "l15.beastade.top" = {
          inherit dnsProvider dnsResolver credentialsFile;
          extraDomainNames = [ "*.l15.beastade.top" ];
        };
      };
  };
  users.users.nginx.extraGroups = [ "acme" ];
}

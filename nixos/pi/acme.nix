{ config, ... }:
{
  sops = {
    templates."acme.env".content = ''
      NAMESILO_API_KEY=${config.sops.placeholder."acme/namesiloApiKey"}
      NAMESILO_PROPAGATION_TIMEOUT=3600
      NAMESILO_POLLING_INTERVAL=120
      NAMESILO_TTL=3600
    '';
    secrets."acme/namesiloApiKey".sopsFile = ./secrets/acme.yaml;
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
        "lan.beastade.top" = {
          inherit dnsProvider dnsResolver credentialsFile;
          extraDomainNames = [ "*.lan.beastade.top" ];
        };
        "wg.beastade.top" = {
          inherit dnsProvider dnsResolver credentialsFile;
          extraDomainNames = [ "*.wg.beastade.top" ];
        };
      };
  };
}

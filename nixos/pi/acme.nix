{ config, ... }:
{
  sops.secrets."acme.env" = {
    sopsFile = ./secrets/acme.env;
    format = "binary";
  };
  security.acme = {
    acceptTerms = true;
    defaults.email = "adomixaszvers@gmail.com";
    certs =
      let
        dnsProvider = "namesilo";
        dnsResolver = "ns1.dnsowl.com:53";
        credentialsFile = config.sops.secrets."acme.env".path;
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

{ config, ... }: {
  sops.secrets."acmeCredentials" = {
    sopsFile = ./secrets/acmeCredentials;
    format = "binary";
  };
  security.acme = {
    acceptTerms = true;
    email = "adomixaszvers@gmail.com";
    certs = let
      dnsProvider = "namesilo";
      dnsResolver = "ns1.dnsowl.com:53";
      credentialsFile = config.sops.secrets."acmeCredentials".path;
    in {
      "lan.beastade.top" = {
        inherit dnsProvider dnsResolver credentialsFile;
        extraDomainNames = [ "*.lan.beastade.top" ];
      };
      "wg.beastade.top" = {
        inherit dnsProvider dnsResolver credentialsFile;
        extraDomainNames = [ "*.wg.beastade.top" ];
      };
      "zt.beastade.top" = {
        inherit dnsProvider dnsResolver credentialsFile;
        extraDomainNames = [ "*.zt.beastade.top" ];
      };
    };
  };
}

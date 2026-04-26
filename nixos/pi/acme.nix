{ config, ... }:
{
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
    certs = {
      "rpi4.beastade.top" = {
        dnsProvider = "namesilo";
        dnsResolver = "ns1.dnsowl.com:53";
        environmentFile = config.sops.templates."acme.env".path;
        extraDomainNames = [ "*.rpi4.beastade.top" ];
      };
    };
  };
}

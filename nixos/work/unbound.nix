{ lib, ... }:
{
  networking.nameservers = lib.mkBefore [ "127.0.0.53" ];
  services.unbound = {
    enable = true;
    enableRootTrustAnchor = false;
    settings = {
      server = {
        interface = "127.0.0.53";
      };
      forward-zone = [
        {
          name = ".";
          forward-addr = [
            "192.168.30.11"
            "192.168.30.12"
          ];
        }
        {
          name = "rpi4.beastade.top.";
          forward-addr = "10.6.0.1";
        }
      ];
    };
  };
}

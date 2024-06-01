{
  networking = {
    domain = "lan";
    nameservers = [ "9.9.9.9" ];
    firewall = {
      allowedTCPPorts = [ 53 6080 ];
      allowedUDPPorts = [ 53 ];
    };
  };
  services.nginx.virtualHosts = let
    locations = { "/" = { proxyPass = "http://192.168.1.207:6080"; }; };
    forceSSL = true;
  in {
    "adguard.lan.beastade.top" = {
      useACMEHost = "lan.beastade.top";
      inherit forceSSL locations;
    };
    "adguard.wg.beastade.top" = {
      useACMEHost = "wg.beastade.top";
      inherit forceSSL locations;
    };
  };

  services.adguardhome = {
    enable = true;
    mutableSettings = true;
    port = 6080;
    host = "192.168.1.207";
    settings = {
      dns = {
        bind_hosts = [ "10.6.0.1" "192.168.1.207" ];
        ratelimit = 0;
        upstream_dns = [ "9.9.9.9" "149.112.112.112" "[/lan/]192.168.1.254" ];
        aaaa_disabled = true;
        local_ptr_upstreams = [ "192.168.1.254" ];
      };
      filtering = {
        blocked_response_ttl = 7200;
        rewrites = [
          {
            domain = "*.wg.beastade.top";
            answer = "10.6.0.1";
          }
          {
            domain = "*.lan.beastade.top";
            answer = "192.168.1.207";
          }
        ];
      };
      filters = [
        {
          enabled = true;
          url = "https://s3.amazonaws.com/lists.disconnect.me/simple_ad.txt";
          name = "Disconnect.me SimpleAd";
        }
        {
          enabled = true;
          url =
            "https://s3.amazonaws.com/lists.disconnect.me/simple_tracking.txt";
          name = "Disconnect.me SimpleTracking";
        }
        {
          enabled = true;
          url = "http://sysctl.org/cameleon/hosts";
          name = "sysctl";
        }
        {
          enabled = true;
          url =
            "https://raw.githubusercontent.com/kevinle-1/Windows-telemetry-blocklist/master/windowsblock.txt";
          name = "Windows Telemetry BlockList";
        }
        {
          enabled = true;
          url =
            "https://raw.githubusercontent.com/StevenBlack/hosts/master/alternates/fakenews-gambling-porn/hosts";
          name = "Unified hosts file with base extensions";
        }
      ];
    };
  };

}

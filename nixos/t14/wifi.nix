{ config, ... }:
{
  sops = {
    secrets =
      let
        secretConf = {
          sopsFile = ../common-secrets/wifi.yaml;
        };
      in
      {
        "wifi/home/ssid" = secretConf;
        "wifi/home/bssid" = secretConf;
        "wifi/home/password" = secretConf;
      };
    templates."wifi.env".content = ''
      HOME_SSID="${config.sops.placeholder."wifi/home/ssid"}"
      HOME_BSSID="${config.sops.placeholder."wifi/home/bssid"}"
      HOME_PASSWORD="${config.sops.placeholder."wifi/home/password"}"
    '';
  };
  networking.networkmanager.ensureProfiles = {
    environmentFiles = [ config.sops.templates."wifi.env".path ];
    profiles = {
      "Home Wifi" = {
        connection = {
          autoconnect-priority = "98";
          id = "Greitas 5GHz";
          timestamp = "1770138378";
          type = "wifi";
          uuid = "af842002-436e-4893-9314-ed7d6e0bae2f";
        };
        ipv4 = {
          address1 = "192.168.1.184/24";
          dns = "192.168.1.207;9.9.9.9;";
          dns-search = "lan;";
          gateway = "192.168.1.254";
          method = "manual";
          route1 = "10.6.0.6/32,192.168.1.207";
        };
        ipv6 = {
          addr-gen-mode = "stable-privacy";
          method = "auto";
        };
        proxy = { };
        wifi = {
          band = "a";
          bssid = "$HOME_BSSID";
          mac-address = "C8:94:02:DD:EE:B3";
          mode = "infrastructure";
          ssid = "$HOME_SSID";
        };
        wifi-security = {
          key-mgmt = "wpa-psk";
          psk = "$HOME_PASSWORD";
        };
      };
    };
  };
}

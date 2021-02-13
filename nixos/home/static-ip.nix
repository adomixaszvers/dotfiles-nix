{
  networking = {
    defaultGateway = "192.168.1.254";
    dhcpcd.enable = false;
    interfaces.enp5s0.ipv4.addresses = [{
      address = "192.168.1.105";
      prefixLength = 24;
    }];
    nameservers = [ "192.168.1.207" ];
    networkmanager.enable = false;
  };
}

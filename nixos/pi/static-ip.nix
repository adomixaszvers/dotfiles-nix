{
  networking.defaultGateway = "192.168.1.254";
  networking.interfaces.eth0 = {
    useDHCP = false;
    ipv4.addresses = [{
      address = "192.168.1.207";
      prefixLength = 24;
    }];
  };
}

{
  networking.defaultGateway = "192.168.1.254";
  networking.interfaces.enp2s0 = {
    useDHCP = false;
    ipv4.addresses = [
      {
        address = "192.168.1.150";
        prefixLength = 24;
      }
    ];
  };
}

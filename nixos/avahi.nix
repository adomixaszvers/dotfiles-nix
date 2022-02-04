{
  services.avahi = {
    enable = true;
    interfaces = [ "enp5s0" "ztzlgoe57z" ];
    nssmdns = true;
    publish = {
      enable = true;
      addresses = true;
      domain = true;
      userServices = true;
      workstation = true;
    };
  };
}

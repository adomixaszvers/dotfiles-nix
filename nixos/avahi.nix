{
  services.avahi = {
    enable = true;
    nssmdns4 = true;
    publish = {
      enable = false;
      addresses = true;
      domain = true;
      userServices = true;
      workstation = true;
    };
  };
}

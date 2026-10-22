{
  services.avahi = {
    enable = true;
    nssmdns4 = true;
    settings.publish = {
      disable-publishing = true;
      publish-addresses = true;
      publish-domain = true;
      publish-workstation = true;
    };
  };
}

{ lib, ... }:
{
  services = {
    fwupd.enable = true;
    fprintd.enable = true;
  };
  security.pam.services.login.fprintAuth = lib.mkDefault false;
}

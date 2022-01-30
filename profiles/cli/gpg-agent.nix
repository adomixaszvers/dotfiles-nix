{ lib, ... }:
{
  services.gpg-agent = {
    enable = lib.mkDefault true;
    enableSshSupport = true;
    enableExtraSocket = true;
  };
}

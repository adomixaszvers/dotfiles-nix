{ lib, pkgs, ... }:
{
  programs.gpg = {
    enable = true;
    settings.keyserver = "keyserver.ubuntu.com";
    scdaemonSettings = {
      reader-port = "Yubico Yubi";
      disable-ccid = true;
    };
  };
  services.gpg-agent = {
    enable = lib.mkDefault true;
    pinentryPackage = pkgs.pinentry-qt;
    enableSshSupport = true;
    enableExtraSocket = true;
  };
}

{ lib, pkgs, ... }:
{
  home.file.".gnupg/gnupg-pkcs11-scd.conf".text = ''
    provider-ykcs11-library "${pkgs.yubico-piv-tool}/lib/libykcs11.so"
  '';
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
    pinentry.package = pkgs.pinentry-qt;
    enableSshSupport = false; # cannot add yubikey piv
    enableExtraSocket = true;
  };
}

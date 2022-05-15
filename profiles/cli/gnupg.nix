{ lib, ... }: {
  programs.gpg = {
    enable = true;
    scdaemonSettings = { reader-port = "Yubico Yubi"; };
  };
  services.gpg-agent = {
    enable = lib.mkDefault true;
    enableSshSupport = true;
    enableExtraSocket = true;
  };
}

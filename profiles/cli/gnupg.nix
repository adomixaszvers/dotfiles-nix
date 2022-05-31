{ lib, ... }: {
  programs.gpg = {
    enable = true;
    scdaemonSettings = {
      reader-port = "Yubico Yubi";
      disable-ccid = true;
    };
  };
  services.gpg-agent = {
    enable = lib.mkDefault true;
    enableSshSupport = true;
    enableExtraSocket = true;
  };
}

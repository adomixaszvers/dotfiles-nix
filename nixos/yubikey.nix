{ pkgs, ... }:
{
  environment.systemPackages = [
    pkgs.yubioath-flutter
    # yubikey-manager-qt
  ];
  hardware.gpgSmartcards.enable = true;
  programs.ssh.extraConfig = ''
    PKCS11Provider "${pkgs.yubico-piv-tool}/lib/libykcs11.so"
  '';
  services = {
    udev.packages = [
      pkgs.yubikey-personalization
      pkgs.libu2f-host
    ];
    pcscd.enable = true;
  };
}

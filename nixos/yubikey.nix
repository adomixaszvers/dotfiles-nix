{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    yubioath-flutter
    # yubikey-manager-qt
  ];
  hardware.gpgSmartcards.enable = true;
  programs.ssh.extraConfig = ''
    PKCS11Provider "${pkgs.yubico-piv-tool}/lib/libykcs11.so"
  '';
  services = {
    udev.packages = with pkgs; [
      yubikey-personalization
      libu2f-host
    ];
    pcscd.enable = true;
  };
}

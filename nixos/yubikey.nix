{ pkgs, ... }: {
  environment.systemPackages = with pkgs; [
    yubioath-desktop
    yubikey-manager-qt
  ];
  hardware.gpgSmartcards.enable = true;
  services = {
    udev.packages = with pkgs; [ yubikey-personalization libu2f-host ];
    pcscd.enable = true;
  };
}

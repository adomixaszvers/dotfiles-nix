{ pkgs, ... }: {
  programs.ssh.askPassword =
    "${pkgs.plasma5Packages.ksshaskpass.out}/bin/ksshaskpass";
  services.power-profiles-daemon.enable = false;
  services.xserver = {
    displayManager.sddm.enable = true;
    desktopManager.plasma5.enable = true;
  };
  security.pam.services.sddm.enableGnomeKeyring = true;
  security.pam.services.sddm.fprintAuth = false;
}

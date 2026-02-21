{ pkgs, ... }:
{
  programs.ssh.askPassword = "${pkgs.kdePackages.ksshaskpass.out}/bin/ksshaskpass";
  services = {
    displayManager = {
      plasma-login-manager.enable = true;
      defaultSession = "plasma";
    };
    desktopManager.plasma6.enable = true;
  };
  security.pam.services.kde.enableGnomeKeyring = true;
}

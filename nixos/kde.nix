{ pkgs, ... }: {
  programs.ssh.askPassword =
    "${pkgs.kdePackages.ksshaskpass.out}/bin/ksshaskpass";
  services = {
    displayManager = {
      sddm = {
        enable = true;
        autoNumlock = true;
      };
      defaultSession = "plasma";
    };
    desktopManager.plasma6.enable = true;
  };
  security.pam.services.kde.enableGnomeKeyring = true;
}

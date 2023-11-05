{ pkgs, ... }: {
  programs.ssh.askPassword =
    "${pkgs.plasma5Packages.ksshaskpass.out}/bin/ksshaskpass";
  services.xserver = {
    displayManager = {
      sddm = {
        enable = true;
        autoNumlock = true;
      };
      defaultSession = "plasmawayland";
    };
    desktopManager.plasma5.enable = true;
  };
  security.pam.services.sddm.enableGnomeKeyring = true;
  security.pam.services.sddm.fprintAuth = false;
}

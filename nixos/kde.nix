{ pkgs, ... }: {
  programs.ssh.askPassword =
    "${pkgs.plasma5Packages.ksshaskpass.out}/bin/ksshaskpass";
  services.xserver = {
    displayManager = {
      lightdm.enable = false;
      sddm.enable = true;
    };
    desktopManager.plasma5.enable = true;
  };
}

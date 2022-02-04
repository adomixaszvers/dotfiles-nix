{ pkgs, ... }: {
  services.xserver = {
    displayManager = {
      lightdm.enable = false;
      gdm.enable = true;
    };
    desktopManager.gnome3.enable = true;
  };
}

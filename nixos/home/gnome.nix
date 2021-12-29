{
  services.xserver = {
    displayManager = {
      lightdm.enable = false;
      gdm.enable = true;
    };
    desktopManager.gnome.enable = true;
  };
}

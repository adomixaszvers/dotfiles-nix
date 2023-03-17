{
  hardware.pulseaudio.enable = false;
  services.power-profiles-daemon.enable = false;
  services.xserver = {
    displayManager.gdm.enable = true;
    desktopManager.gnome.enable = true;
  };
}

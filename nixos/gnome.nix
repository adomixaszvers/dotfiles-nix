{
  services = {
    pulseaudio.enable = false;
    power-profiles-daemon.enable = false;
    xserver = {
      displayManager.gdm.enable = true;
      desktopManager.gnome.enable = true;
    };
  };
}

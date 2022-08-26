{
  environment.sessionVariables.MOZ_ENABLE_WAYLAND = "1";
  services.xserver = {
    displayManager.gdm.enable = true;
    desktopManager.gnome.enable = true;
  };
}

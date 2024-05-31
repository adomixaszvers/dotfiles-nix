{ lib, pkgs, myPkgs, config, ... }:

{
  gtk = {
    cursorTheme = {
      package = pkgs.vanilla-dmz;
      name = "Vanilla-DMZ";
      size = 32;
    };
    iconTheme = {
      name = "Arc";
      package = pkgs.arc-icon-theme;
    };
    gtk3.extraConfig = { gtk-application-prefer-dark-theme = true; };
  };
  qt = {
    enable = lib.mkDefault true;
    platformTheme.name = "gtk";
  };
  services.network-manager-applet.enable = lib.mkDefault true;
  services.udiskie = {
    enable = lib.mkDefault true;
    automount = false;
  };
  stylix.targets.gtk.enable = lib.mkDefault true;
  home.packages = with myPkgs; [
    maimpick
    (rofi-powermenu.override { rofi = config.programs.rofi.finalPackage; })
  ];
  home.sessionVariables =
    let askpass = "${pkgs.gnome.seahorse}/libexec/seahorse/ssh-askpass";
    in {
      SSH_ASKPASS = askpass;
      SUDO_ASKPASS = askpass;
    };
}

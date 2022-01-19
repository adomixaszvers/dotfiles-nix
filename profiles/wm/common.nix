{ lib, pkgs, myPkgs, ... }:

{
  imports = [ ./sx.nix ];
  gtk = {
    enable = lib.mkDefault true;
    iconTheme = {
      name = "Arc";
      package = pkgs.arc-icon-theme;
    };
    theme = {
      name = "Arc-Darker";
      package = pkgs.arc-theme;
    };
    gtk3.extraConfig = { gtk-application-prefer-dark-theme = true; };
  };
  qt = {
    enable = lib.mkDefault true;
    platformTheme = "gtk";
  };
  services.network-manager-applet.enable = lib.mkDefault true;
  services.udiskie = {
    enable = true;
    automount = false;
  };
  home.packages = with myPkgs; [ maimpick rofi-powermenu ];
  home.sessionVariables =
    let askpass = "${pkgs.gnome.seahorse}/libexec/seahorse/ssh-askpass";
    in {
      SSH_ASKPASS = askpass;
      SUDO_ASKPASS = askpass;
    };
}

{ lib, pkgs, ... }:

{
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
  home.packages = with pkgs; [ mine.maimpick mine.rofi-powermenu ];
  home.sessionVariables =
    let askpass = "${pkgs.gnome3.seahorse}/libexec/seahorse/ssh-askpass";
    in {
      SSH_ASKPASS = askpass;
      SUDO_ASKPASS = askpass;
    };
}

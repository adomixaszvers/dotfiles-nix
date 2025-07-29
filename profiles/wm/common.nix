{
  lib,
  pkgs,
  myPkgs,
  config,
  ...
}:

{
  gtk = {
    iconTheme = {
      name = "Arc";
      package = pkgs.arc-icon-theme;
    };
  };
  qt = {
    enable = lib.mkDefault true;
    platformTheme.name = "adwaita";
  };
  services.network-manager-applet.enable = lib.mkDefault true;
  services.udiskie = {
    enable = lib.mkDefault true;
    automount = false;
  };
  stylix.targets = {
    gnome.enable = lib.mkDefault true;
    gtk.enable = lib.mkDefault true;
  };
  home.packages = with myPkgs; [
    maimpick
    (rofi-powermenu.override { rofi = config.programs.rofi.finalPackage; })
  ];
  home.sessionVariables =
    let
      askpass = "${pkgs.seahorse}/libexec/seahorse/ssh-askpass";
    in
    {
      SSH_ASKPASS = askpass;
      SUDO_ASKPASS = askpass;
    };
}

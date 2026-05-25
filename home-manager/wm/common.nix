{
  lib,
  pkgs,
  myPkgs,
  config,
  inputs,
  ...
}:

{
  imports = [
    (inputs.nix-wrapper-modules.lib.getInstallModule {
      name = "niri";
      value = inputs.self.wrapperModules.niri;
    })
  ];
  gtk = {
    gtk4.theme = null;
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
  stylix = {
    icons.enable = lib.mkDefault true;
    targets = {
      gtk.enable = lib.mkDefault true;
    };
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

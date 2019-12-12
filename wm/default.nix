{ config, lib, pkgs, ... }:

{
  imports = [ ./xmonad ./polybar.nix ./dunst.nix ./compton.nix ];
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
  services.udiskie.enable = true;
  xsession.enable = lib.mkDefault true;
  xsession.initExtra = ''
    xset s off -dpms
  '';
  xsession.pointerCursor = {
    name = "capitaine-cursors";
    package = pkgs.capitaine-cursors;
  };
  xsession.scriptPath = ".xsession-hm";
  home.sessionVariables =
    let askpass = "${pkgs.lxqt.lxqt-openssh-askpass}/bin/lxqt-openssh-askpass";
    in {
      SSH_ASKPASS = askpass;
      SUDO_ASKPASS = askpass;
    };
}

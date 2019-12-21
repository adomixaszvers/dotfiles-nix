{ config, lib, pkgs, ... }:

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
  services.screen-locker = {
    inactiveInterval = 5;
    lockCmd = lib.mkDefault "i3lock -n -t -f";
    xautolockExtraOptions = [
      "-corners"
      "--00"
      "-killer"
      "'systemctl hybric-sleep'"
      "-killtime"
      "20"
    ];
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
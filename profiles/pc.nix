{ pkgs, myPkgs, ... }: {
  imports = [ ./common.nix ];
  colors = import ./gui/colors/nord.nix;
  home.packages = (with pkgs; [
    bitwarden
    brightnessctl
    borgbackup
    compsize
    discord
    exercism
    gnome.nautilus
    firefox
    gtypist
    jetbrains.gateway
    libreoffice-still
    lutris
    mpv
    playerctl
    remmina
    qbittorrent
    spotify
    tor-browser-bundle-bin
    keepassxc
    xpra
    protonup
  ]) ++ (with myPkgs; [ ani-cli ]);
  home.sessionVariables = { BROWSER = "firefox"; };
  gtk.enable = false;
  qt.enable = false;
  programs.xmobar.thermal-zone = 1;
  services.network-manager-applet.enable = false;
  services.udiskie.enable = false;
  xsession.initExtra = ''
    xset s off -dpms
    xrandr --output eDP --set TearFree on
  '';
}

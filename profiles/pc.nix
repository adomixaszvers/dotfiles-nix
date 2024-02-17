{ pkgs, ... }: {
  imports = [ ./common.nix ];
  colors = import ./gui/colors/nord.nix;
  home.packages = with pkgs; [
    discord
    firefox
    # gamescope
    libreoffice-still
    (lutris.override {
      extraPkgs = ps:
        with ps; [
          wine
          xorg.libXcursor
          xorg.libXi
          xorg.libXinerama
          xorg.libXScrnSaver
          libpng
          # libpulseaudio
          libvorbis
          stdenv.cc.cc.lib
          libkrb5
          keyutils
          mangohud
        ];
    })
    mpv
    obs-studio
    playerctl
    remmina
    qbittorrent
    keepassxc
    xpra
    protonup-qt
  ];
  home.sessionVariables = { BROWSER = "firefox"; };
  gtk.enable = false;
  qt.enable = false;
  gui.thermal-zone = 2;
  programs = {
    # kitty.settings.shell = "nu";
    nushell.enable = true;
  };
  services.network-manager-applet.enable = false;
  services.udiskie.enable = false;
  xsession.initExtra = ''
    xset s off -dpms
    xrandr --output eDP --set TearFree on
  '';
}

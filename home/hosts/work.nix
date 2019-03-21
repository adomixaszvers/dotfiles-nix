{ pkgs, ... }:
{
  imports = [ ../emacs.nix ];
  home.packages = with pkgs;
  [
    # mine.consul
    # yarn
    docker
    filezilla
    flameshot
    google-chrome
    jdk8
    jetbrains.idea-ultimate
    jq
    libreoffice-fresh
    maven
    mercurial
    nodejs
    robo3t
    skype
    smartgithg
    sqldeveloper_18
    traceroute
    unzip
    visualvm
    whois
  ];
  home.sessionVariables = {
    BROWSER = "${pkgs.google-chrome}/bin/google-chrome-stable";
  };
  services.screen-locker = {
    enable = true;
    inactiveInterval = 5;
    lockCmd = "i3lock -n";
  };
  xsession.windowManager.i3.config.startup = [
    # add services.flatpak.enable = true in OS configuration
    # flatpak --user remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
    # flatpak --user install flathub chat.rocket.RocketChat
    { command = "flatpak run chat.rocket.RocketChat"; notification = false; }
  ];
}

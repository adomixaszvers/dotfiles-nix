{ pkgs, ... }:
{
  imports = [ ../emacs.nix ];
  home.packages = with pkgs;
  [
    docker
    flameshot
    google-chrome
    jdk8
    jetbrains.idea-ultimate
    jq
    libreoffice-fresh
    maven
    # mine.consul
    nodejs
    robo3t
    # skype
    smartgithg
    traceroute
    unzip
    visualvm
    whois
    # yarn
  ];
  home.sessionVariables = {
    BROWSER = "${pkgs.google-chrome}/bin/google-chrome-stable";
  };
  services.screen-locker = {
    enable = true;
    inactiveInterval = 5;
    lockCmd = "i3lock -n";
  };
}

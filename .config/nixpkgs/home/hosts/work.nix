{ pkgs, ... }:
{
  home.packages = with pkgs;
  [
    docker
    google-chrome
    flameshot
    jdk8
    jetbrains.idea-ultimate
    libreoffice-fresh
    maven
    mine.consul
    nodejs
    robo3t
    skype
    smartgithg
    traceroute
    visualvm
    whois
    yarn
  ];
  home.sessionVariables = {
    BROWSER = "${pkgs.google-chrome}/bin/google-chrome-stable";
  };
  xsession.windowManager.i3.config.startup = [
    { command = "skypeforlinux"; notification = false; }
  ];
}

{ pkgs, ... }:
{
  home.packages = with pkgs;
  [
    docker
    google-chrome
    jdk8
    jetbrains.idea-ultimate
    libreoffice-fresh
    maven
    mine.consul
    nodejs
    robo3t
    skype
    smartgithg
    visualvm
    yarn
  ];
  xsession.windowManager.i3.config.startup = [
    { command = "skypeforlinux"; notification = false; }
  ];
}

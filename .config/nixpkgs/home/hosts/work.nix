{ pkgs, ... }:
{
  home.packages = with pkgs;
  [
    docker
    visualvm
    mine.consul
    jdk8
    jetbrains.idea-ultimate
    libreoffice-fresh
    maven
    nodejs
    yarn
    smartgithg
    skype
  ];
}

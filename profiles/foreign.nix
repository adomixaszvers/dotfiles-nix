{ pkgs, ... }:
{
  imports = [ ./cli ];
  home.packages = with pkgs; [
    hostname
    nix
  ];
  services.gpg-agent.enable = false;
  xdg = {
    enable = true;
  };
  targets.genericLinux.enable = true;
}

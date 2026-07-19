{ pkgs, ... }:
{
  imports = [ ./cli ];
  home.packages = with pkgs; [
    # keep-sorted start
    hostname
    nix
    # keep-sorted end
  ];
  services.gpg-agent.enable = false;
  xdg = {
    enable = true;
  };
  targets.genericLinux.enable = true;
}

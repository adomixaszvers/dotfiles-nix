{ pkgs, ... }:
{
  imports = [ ./cli ];
  home.packages = [
    # keep-sorted start
    pkgs.hostname
    pkgs.nix
    # keep-sorted end
  ];
  services.gpg-agent.enable = false;
  xdg = {
    enable = true;
  };
  targets.genericLinux.enable = true;
}

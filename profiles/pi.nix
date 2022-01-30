{ lib, pkgs, ... }: {
  imports = [ ../modules ./cli ];
  services.gpg-agent.enable = false;
  xdg = { enable = true; };
}

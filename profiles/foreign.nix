{ lib, pkgs, ... }: {
  imports = [ ../modules ./cli ];
  home = {
    stateVersion = "20.09";
    packages = with pkgs; [ hostname nix_2_4 ];
  };
  services.gpg-agent.enable = lib.mkForce false;
  xdg = { enable = true; };
  targets.genericLinux.enable = true;
}

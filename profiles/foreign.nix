{ pkgs, ... }: {
  imports = [ ./cli ];
  home.packages = with pkgs; [ hostname nix_2_4 ];
  services.gpg-agent.enable = false;
  xdg = { enable = true; };
  targets.genericLinux.enable = true;
}

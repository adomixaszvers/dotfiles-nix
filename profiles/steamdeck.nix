{ pkgs, ... }: {
  imports = [ ./cli ];
  home.packages = with pkgs; [ firefox keepassxc ];
  services.gpg-agent.enable = false;
  xdg = { enable = true; };
  targets.genericLinux.enable = true;
}

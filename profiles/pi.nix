{
  imports = [ ./cli ];
  services.gpg-agent.enable = false;
  nixCats.packageNames = [ "nixCats-small" ];
  xdg = {
    enable = true;
  };
}

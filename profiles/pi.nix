{
  imports = [ ./cli ];
  services.gpg-agent.enable = false;
  nixCats.packageNames = [ "nvim" ];
  xdg = {
    enable = true;
  };
}

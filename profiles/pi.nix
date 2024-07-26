{
  imports = [ ./cli ];
  services.gpg-agent.enable = false;
  xdg = {
    enable = true;
  };
}

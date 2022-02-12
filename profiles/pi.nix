{
  imports = [ ./cli bad ];
  services.gpg-agent.enable = false;
  xdg = { enable = true; };
}

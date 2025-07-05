{
  imports = [
    ./cli
    ./cli/jujutsu.nix
  ];
  services.gpg-agent.enable = false;
  nixCats.packageNames = [ "nixCats-small" ];
  xdg = {
    enable = true;
  };
}

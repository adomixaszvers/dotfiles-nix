{
  imports = [
    ./cli
    ./cli/atuin
    ./cli/jujutsu.nix
  ];
  services.gpg-agent.enable = false;
  nixCats.packageNames = [ "nixCats-small" ];
  programs.atuin.settings.sync_address = "http://127.0.0.1:8090";
  xdg = {
    enable = true;
  };
}

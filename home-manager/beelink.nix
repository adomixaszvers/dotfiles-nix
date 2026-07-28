{
  imports = [
    ./cli
    ./cli/atuin
    ./cli/jujutsu.nix
  ];
  programs.atuin.settings.sync_address = "http://127.0.0.1:8090";
  services.gpg-agent.enable = false;
  xdg = {
    enable = true;
  };
  wrappers = {
    neovim.enable = true;
    neovim-nix.enable = false;
  };
}

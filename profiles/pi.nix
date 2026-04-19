{
  imports = [
    ./cli
    ./cli/atuin
    ./cli/jujutsu.nix
  ];
  services.gpg-agent.enable = false;
  programs.atuin.settings.sync_address = "http://127.0.0.1:8090";
  xdg = {
    enable = true;
  };
  wrappers = {
    neovim.enable = false;
    neovim-nix.enable = true;
  };
}

{
  imports = [
    ./cli
    ./cli/jujutsu.nix
  ];
  services.gpg-agent.enable = false;
  xdg = {
    enable = true;
  };
  wrappers = {
    neovim.enable = true;
    neovim-nix.enable = false;
  };
}

{
  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
    nix-direnv = { enable = true; };
    stdlib = builtins.readFile ./stdlib.bash;
  };
}

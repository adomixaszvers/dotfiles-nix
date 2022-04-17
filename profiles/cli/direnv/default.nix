{ unstable, ... }: {
  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
    stdlib = ''
      source ${unstable.nix-direnv}/share/nix-direnv/direnvrc
    '';
    # nix-direnv = {
    #   enable = true;
    #   enableFlakes = true;
    # };
  };
}

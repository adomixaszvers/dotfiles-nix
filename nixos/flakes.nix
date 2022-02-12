{ pkgs, ... }:
let nixPackage = pkgs.nix_2_4;
in {
  nix = {
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    package = nixPackage;
  };
  # programs.zsh.interactiveShellInit = ''
  #   source ${nixPackage.src}/misc/zsh/completion.zsh
  # '';
}

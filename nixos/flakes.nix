{ pkgs, ... }:
let nixPackage = pkgs.nixFlakes;
in {
  nix = {
    extraOptions = ''
      experimental-features = nix-command flakes ca-references
    '';
    package = nixPackage;
  };
  programs.zsh.interactiveShellInit = ''
    source ${nixPackage.src}/misc/zsh/completion.zsh
  '';
}

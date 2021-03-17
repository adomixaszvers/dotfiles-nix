{ pkgs, ... }:
let
  nixPackage = pkgs.nixFlakes;
  flake-compat = builtins.fetchTarball
    "https://github.com/edolstra/flake-compat/archive/master.tar.gz";
  adomoFlakes = import flake-compat { src = /home/adomas/.config/nixpkgs; };
in {
  nix = {
    extraOptions = ''
      experimental-features = nix-command flakes ca-references
    '';
    package = nixPackage;
    registry = with adomoFlakes.defaultNix.inputs; {
      nixpkgs.flake = nixpkgs;
      nixos-unstable.flake = nixos-unstable;
    };
  };
  programs.zsh.interactiveShellInit = ''
    source ${nixPackage.src}/misc/zsh/completion.zsh
  '';
}

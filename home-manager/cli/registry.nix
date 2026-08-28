{ inputs, config, ... }:
{
  nix.registry = {
    mine.to = {
      type = "git";
      url = "file://${config.home.homeDirectory}/.config/nixpkgs";
    };
    nixpkgs.flake = inputs.nixpkgs;
    nixos-unstable.flake = inputs.nixos-unstable;
    master.to = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
    };
  };
}

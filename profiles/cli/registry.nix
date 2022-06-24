{ inputs, config, ... }: {
  nix.registry = with inputs; {
    mine.to = {
      type = "git";
      url = "file://${config.home.homeDirectory}/.config/nixpkgs";
    };
    nixpkgs.flake = nixpkgs;
    nixos-unstable.flake = nixos-unstable;
    master.to = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
    };
  };
}

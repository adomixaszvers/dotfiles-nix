{ inputs, ... }: {
  nix.registry = with inputs; {
    mine.to = {
      type = "git";
      url = "file:///home/adomas/.config/nixpkgs";
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

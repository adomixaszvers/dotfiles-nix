{ inputs, ... }: {
  nix.registry = with inputs; {
    mine.flake = self;
    nixpkgs.flake = nixpkgs;
    nixos-unstable.flake = nixos-unstable;
  };
}

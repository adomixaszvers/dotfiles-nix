{ inputs, ... }: {
  nix.registry = with inputs; {
    nixpkgs.flake = nixpkgs;
    nixos-unstable.flake = nixos-unstable;
  };
}

{ inputs, ... }:
let inherit (inputs) nixpkgs nixos-hardware sops-nix;
in {
  nix = {
    nixPath = [
      "nixpkgs=${nixpkgs}"
      "nixos-hardware=${nixos-hardware}"
      "sops-nix=${sops-nix}"
    ];
    registry = {
      nixpkgs.flake = nixpkgs;
      nixos-hardware.flake = nixos-hardware;
      sops-nix.flake = sops-nix;
    };
  };
}

{ nixpkgs, nixos-hardware, inputs }:
let
  nixRegistry = {
    nix = {
      nixPath = [ "nixpkgs=${nixpkgs}" "nixos-hardware=${nixos-hardware}" ];
      registry = {
        nixpkgs.flake = nixpkgs;
        nixos-hardware.flake = nixos-hardware;
      };
    };
  };
  common = [ nixRegistry ./common.nix ./doas.nix ./flakes.nix ./yubikey.nix ];
in {
  adomo-nixos = nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs = { inherit inputs; };
    modules = common ++ [
      nixpkgs.nixosModules.notDetected
      nixos-hardware.nixosModules.common-cpu-intel
      ./home/configuration.nix
    ];
  };
}

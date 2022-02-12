{ inputs }:
let
  inherit (inputs) nixpkgs nixos-hardware sops-nix;
  specialArgs = { inherit inputs; };
in {
  adomo-nixos = nixpkgs.lib.nixosSystem {
    inherit specialArgs;
    system = "x86_64-linux";
    modules = [
      ./common.nix
      ./flakes.nix
      ./home/configuration.nix
      ./nix-registry.nix
      sops-nix.nixosModules.sops
      nixos-hardware.nixosModules.common-cpu-intel
      nixpkgs.nixosModules.notDetected
    ];
  };
  adomas-jatuzis-nixos = nixpkgs.lib.nixosSystem {
    inherit specialArgs;
    system = "x86_64-linux";
    modules = [
      ./common.nix
      ./flakes.nix
      ./nix-registry.nix
      ./work/configuration.nix
      sops-nix.nixosModules.sops
      nixos-hardware.nixosModules.common-cpu-intel
      nixpkgs.nixosModules.notDetected
    ];
  };
  raspberrypi-nixos = nixpkgs.lib.nixosSystem {
    inherit specialArgs;
    system = "aarch64-linux";
    modules = [
      ./flakes.nix
      ./nix-registry.nix
      ./pi/configuration.nix
      sops-nix.nixosModules.sops
      nixos-hardware.nixosModules.raspberry-pi-4
    ];
  };
}

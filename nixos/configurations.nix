{ inputs }:
let
  inherit (inputs) nixpkgs nixos-hardware sops-nix;
  sops-common = { sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ]; };
  nixRegistry = {
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
  };
  common = [ sops-common nixRegistry ./common.nix ./flakes.nix ];
in {
  adomo-nixos = nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs = { inherit inputs; };
    modules = common ++ [
      nixpkgs.nixosModules.notDetected
      nixos-hardware.nixosModules.common-cpu-intel
      ./home/configuration.nix
    ];
    extraModules = [ sops-nix.nixosModules.sops ];
  };
}

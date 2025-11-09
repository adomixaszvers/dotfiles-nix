{ inputs, ... }:
let
  inherit (inputs) nixpkgs;
  specialArgs = {
    inherit inputs;
  };
in
{
  flake.nixosConfigurations = {
    adomo-nixos = nixpkgs.lib.nixosSystem {
      inherit specialArgs;
      modules = [
        ./home/configuration.nix
        ./ssh-workaround.nix
      ];
    };
    adomas-jatuzis-nixos = nixpkgs.lib.nixosSystem {
      inherit specialArgs;
      modules = [
        ./work/configuration.nix
        ./ssh-workaround.nix
      ];
    };
    adomo-pc-nixos = nixpkgs.lib.nixosSystem {
      inherit specialArgs;
      modules = [
        ./pc/configuration.nix
        ./ssh-workaround.nix
      ];
    };
    adomo-t14 = nixpkgs.lib.nixosSystem {
      inherit specialArgs;
      modules = [
        ./t14/configuration.nix
        ./ssh-workaround.nix
      ];
    };
    raspberrypi-nixos = nixpkgs.lib.nixosSystem {
      inherit specialArgs;
      modules = [
        ./pi/configuration.nix
        ./ssh-workaround.nix
      ];
    };

  };
}

{ inputs, ... }:
let
  inherit (inputs) nixpkgs determinate;
  specialArgs = {
    inherit inputs;
  };
in
{
  flake.nixosConfigurations = {
    adomo-nixos = nixpkgs.lib.nixosSystem {
      inherit specialArgs;
      modules = [
        determinate.nixosModules.default
        ./home/configuration.nix
      ];
    };
    adomas-jatuzis-nixos = nixpkgs.lib.nixosSystem {
      inherit specialArgs;
      modules = [
        determinate.nixosModules.default
        ./work/configuration.nix
      ];
    };
    adomo-pc-nixos = nixpkgs.lib.nixosSystem {
      inherit specialArgs;
      modules = [
        determinate.nixosModules.default
        ./pc/configuration.nix
      ];
    };
    adomo-t14 = nixpkgs.lib.nixosSystem {
      inherit specialArgs;
      modules = [
        determinate.nixosModules.default
        ./t14/configuration.nix
      ];
    };
    raspberrypi-nixos = nixpkgs.lib.nixosSystem {
      inherit specialArgs;
      modules = [
        determinate.nixosModules.default
        ./pi/configuration.nix
      ];
    };

  };
}

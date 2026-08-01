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
        ./asus-laptop/configuration.nix
      ];
    };
    adomas-jatuzis-nixos = nixpkgs.lib.nixosSystem {
      inherit specialArgs;
      modules = [
        ./work/configuration.nix
      ];
    };
    adomo-pc-nixos = nixpkgs.lib.nixosSystem {
      inherit specialArgs;
      modules = [
        ./pc/configuration.nix
      ];
    };
    adomo-t14 = nixpkgs.lib.nixosSystem {
      inherit specialArgs;
      modules = [
        ./t14/configuration.nix
      ];
    };
    beelink = nixpkgs.lib.nixosSystem {
      inherit specialArgs;
      modules = [
        ./beelink/configuration.nix
      ];
    };
    # raspberrypi-nixos = nixpkgs.lib.nixosSystem {
    #   inherit specialArgs;
    #   modules = [
    #     ./pi/configuration.nix
    #   ];
    # };

  };
}

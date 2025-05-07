{ inputs, ... }:
let
  inherit (inputs) nixpkgs;
  specialArgs = {
    inherit inputs;
  };
  isoSpecialArgs = {
    inherit inputs;
    myPkgs = inputs.self.packages.x86_64-linux;
  };
in
{
  flake.nixosConfigurations = {
    adomo-nixos = nixpkgs.lib.nixosSystem {
      inherit specialArgs;
      modules = [ ./home/configuration.nix ];
    };
    adomas-jatuzis-nixos = nixpkgs.lib.nixosSystem {
      inherit specialArgs;
      modules = [ ./work/configuration.nix ];
    };
    adomo-pc-nixos = nixpkgs.lib.nixosSystem {
      inherit specialArgs;
      modules = [ ./pc/configuration.nix ];
    };
    adomo-t14 = nixpkgs.lib.nixosSystem {
      inherit specialArgs;
      modules = [ ./t14/configuration.nix ];
    };
    raspberrypi-nixos = nixpkgs.lib.nixosSystem {
      inherit specialArgs;
      modules = [ ./pi/configuration.nix ];
    };

    # build with `nix build '.#nixosConfigurations.iso-minimal.config.system.build.isoImage'`
    iso-minimal = nixpkgs.lib.nixosSystem {
      specialArgs = isoSpecialArgs;
      system = "x86_64-linux";
      modules = [ ./installers/minimal.nix ];
    };
    # build with `nix build '.#nixosConfigurations.iso-plasma6.config.system.build.isoImage'`
    iso-plasma6 = nixpkgs.lib.nixosSystem {
      specialArgs = isoSpecialArgs;
      system = "x86_64-linux";
      modules = [ ./installers/plasma6.nix ];
    };
  };
}

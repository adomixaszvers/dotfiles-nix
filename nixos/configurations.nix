{ inputs }:
let
  inherit (inputs) nixpkgs;
  specialArgs = { inherit inputs; };
  isoSpecialArgs = {
    inherit inputs;
    myPkgs = inputs.self.packages.x86_64-linux;
  };
in {
  adomo-nixos = nixpkgs.lib.nixosSystem {
    inherit specialArgs;
    system = "x86_64-linux";
    modules = [ ./home/configuration.nix ];
  };
  adomas-jatuzis-nixos = nixpkgs.lib.nixosSystem {
    inherit specialArgs;
    system = "x86_64-linux";
    modules = [ ./work/configuration.nix ];
  };
  adomo-pc-nixos = nixpkgs.lib.nixosSystem {
    inherit specialArgs;
    system = "x86_64-linux";
    modules = [ ./pc/configuration.nix ];
  };
  adomo-t14 = nixpkgs.lib.nixosSystem {
    inherit specialArgs;
    system = "x86_64-linux";
    modules = [ ./t14/configuration.nix ];
  };
  raspberrypi-nixos = nixpkgs.lib.nixosSystem {
    inherit specialArgs;
    system = "aarch64-linux";
    modules = [ ./pi/configuration.nix ];
  };

  # build with `nix build '.#nixosConfigurations.iso-minimal.config.system.build.isoImage'`
  iso-minimal = nixpkgs.lib.nixosSystem {
    specialArgs = isoSpecialArgs;
    system = "x86_64-linux";
    modules = [ ./installers/minimal.nix ];
  };
  # build with `nix build '.#nixosConfigurations.iso-plasma5.config.system.build.isoImage'`
  iso-plasma5 = nixpkgs.lib.nixosSystem {
    specialArgs = isoSpecialArgs;
    system = "x86_64-linux";
    modules = [ ./installers/plasma5.nix ];
  };
}

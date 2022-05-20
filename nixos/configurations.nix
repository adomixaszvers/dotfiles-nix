{ inputs }:
let
  inherit (inputs) nixpkgs nixos-hardware sops-nix;
  specialArgs = { inherit inputs; };
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
}

{
  inputs = {
    parent.url = "path:..";
    nixpkgs.follows = "parent/nixpkgs";
    sops-nix.follows = "parent/sops-nix";
    nixos-hardware.follows = "parent/nixos-hardware";
  };
  outputs =
    { nixpkgs, parent, ... }@inputs:
    let
      isoSpecialArgs = {
        inherit inputs;
        myPkgs = inputs.parent.packages.x86_64-linux;
      };
    in
    {
      nixosConfigurations = {

        # build with `nix build './installers/#nixosConfigurations.iso-minimal.config.system.build.isoImage'` --reference-lock-file flake.lock
        iso-minimal = nixpkgs.lib.nixosSystem {
          specialArgs = isoSpecialArgs;
          system = "x86_64-linux";
          modules = [ ./minimal.nix ];
        };
        # build with `nix build './installers/#nixosConfigurations.iso-plasma6.config.system.build.isoImage'` --reference-lock-file flake.lock
        iso-plasma6 = nixpkgs.lib.nixosSystem {
          specialArgs = isoSpecialArgs;
          system = "x86_64-linux";
          modules = [ ./plasma6.nix ];
        };
      };
    };
}

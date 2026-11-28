{ inputs, pkgs, ... }:
{
  imports = [
    inputs.home-manager.nixosModules.home-manager
  ];
  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    extraSpecialArgs = {
      inherit inputs;
      myPkgs = builtins.getAttr pkgs.stdenv.hostPlatform.system inputs.self.packages;
    };
    users.adomas = {
      imports = [
        ../../home-manager/pc.nix
        ../../home-manager/modules
        inputs.nix-index-database.homeModules.nix-index
      ];
      home.stateVersion = "25.05";
    };
  };
}

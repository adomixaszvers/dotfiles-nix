{ inputs, ... }:
{
  imports = [
    inputs.home-manager.nixosModules.home-manager
  ];
  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    extraSpecialArgs = {
      inherit inputs;
      myPkgs = inputs.self.packages.aarch64-linux;
    };
    users.adomas = {
      imports = [ ../../home-manager/beelink.nix ];
      home.stateVersion = "25.05";
    };
  };
}

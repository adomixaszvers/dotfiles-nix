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
    users.pi = {
      imports = [ ../../home-manager/pi.nix ];
      home.stateVersion = "25.05";
    };
  };
}

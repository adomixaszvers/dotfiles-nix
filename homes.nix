{ inputs, withSystem, ... }:

let
  inherit (inputs) home-manager;
  buildHomeManager =
    configuration:
    {
      system ? "x86_64-linux",
      username ? "adomas",
      homeDirectory ? "/home/${username}",
    }:
    withSystem system (
      {
        pkgs,
        unstable,
        self',
        ...
      }:
      home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          ./modules
          {
            home = {
              inherit username homeDirectory;
              stateVersion = "25.05";
            };
          }
          inputs.nix-index-database.homeModules.nix-index
          configuration
        ];
        extraSpecialArgs = {
          inherit inputs unstable system;
          myPkgs = self'.packages;
        };
      }
    );
  work = buildHomeManager ./profiles/work.nix { };
  work-remote = buildHomeManager ./profiles/work-remote.nix { };
  home = buildHomeManager ./profiles/home.nix { };
  t14 = buildHomeManager ./profiles/t14.nix { };
  foreign = buildHomeManager ./profiles/foreign.nix { };
  pi = buildHomeManager ./profiles/pi.nix {
    username = "pi";
    system = "aarch64-linux";
  };
  pc = buildHomeManager ./profiles/pc.nix { };
  thinkpad-home = work-remote;
  deck = buildHomeManager ./profiles/steamdeck.nix { username = "deck"; };
  thinkpad-work = work;
in
{
  flake = {
    homeConfigurations = {
      inherit
        work
        work-remote
        home
        t14
        foreign
        pi
        pc
        thinkpad-home
        deck
        thinkpad-work
        ;
      "adomas@adomo-nixos" = home;
      "adomas@adomo-t14" = t14;
      "adomas@arch-vm" = foreign;
      "adomas@adomas-jatuzis-nixos" = work;
      "adomas@adomo-pc-nixos" = pc;
      "pi@raspberrypi-nixos" = pi;
      "deck@steamdeck" = deck;
    };
    buildbotJobs = {
      "aarch64-linux" = {
        home-manager-pi = pi.activationPackage;
      };
      "x86_64-linux" = {
        home-manager-work = work.activationPackage;
        home-manager-work-remote = work-remote.activationPackage;
        home-manager-home = home.activationPackage;
        home-manager-t14 = t14.activationPackage;
        home-manager-foreign = foreign.activationPackage;
        home-manager-pc = pc.activationPackage;
        home-manager-deck = deck.activationPackage;
      };
    };
  };
}

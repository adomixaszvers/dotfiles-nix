{ inputs, withSystem, ... }:

let
  inherit (inputs) home-manager;
  buildHomeManager = configuration:
    { system ? "x86_64-linux", username ? "adomas"
    , homeDirectory ? "/home/${username}" }:
    withSystem system ({ pkgs, unstable, self', ... }:
      home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          ./modules
          {
            home = {
              inherit username homeDirectory;
              stateVersion = "24.05";
            };
          }
          inputs.nix-index-database.hmModules.nix-index
          configuration
        ];
        extraSpecialArgs = {
          inherit inputs unstable system;
          myPkgs = self'.packages;
        };
      });
in {
  flake.homeConfigurations = rec {
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
    "adomas@adomo-nixos" = home;
    "adomas@adomo-t14" = t14;
    "adomas@arch-vm" = foreign;
    "adomas@adomas-jatuzis-nixos" = work;
    "adomas@adomo-pc-nixos" = pc;
    "pi@raspberrypi-nixos" = pi;
    thinkpad-home = work-remote;
    "deck@steamdeck" =
      buildHomeManager ./profiles/steamdeck.nix { username = "deck"; };
    thinkpad-work = work;
  };
}

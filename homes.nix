{ inputs }:

let
  inherit (inputs) home-manager nixpkgs nixos-unstable self;
  buildHomeManager = config:
    { system ? "x86_64-linux", username ? "adomas"
    , homeDirectory ? "/home/${username}", nixpkgs-config ? import ./config.nix
    , pkgs ? import nixpkgs {
      inherit system;
      config = nixpkgs-config;
    }, unstable ? import nixos-unstable {
      inherit system;
      config = nixpkgs-config;
    }, myPkgs ? builtins.getAttr system self.packages }:
    home-manager.lib.homeManagerConfiguration {
      inherit system homeDirectory username pkgs;
      stateVersion = "22.05";
      extraModules = [ ./modules ];
      extraSpecialArgs = { inherit inputs myPkgs unstable system; };
      configuration = {
        imports = [ config ];
        nixpkgs = { config = nixpkgs-config; };
      };
    };
in rec {
  work = buildHomeManager ./profiles/work.nix { };
  work-remote = buildHomeManager ./profiles/work-remote.nix { };
  home = buildHomeManager ./profiles/home.nix { };
  t14 = buildHomeManager ./profiles/t14.nix { };
  foreign = buildHomeManager ./profiles/foreign.nix { };
  pi = buildHomeManager ./profiles/pi.nix {
    username = "pi";
    system = "aarch64-linux";
  };
  "adomas@adomo-nixos" = home;
  "adomas@adomo-t14" = t14;
  "adomas@arch-vm" = foreign;
  "adomas@adomas-jatuzis-nixos" = work;
  "pi@raspberrypi-nixos" = pi;
  thinkpad-home = work-remote;
  thinkpad-work = work;
}

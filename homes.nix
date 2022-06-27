{ inputs, allPkgs, allUnstablePkgs }:

let
  inherit (inputs) home-manager nixpkgs nixos-unstable self;
  buildHomeManager = configuration:
    { system ? "x86_64-linux", username ? "adomas"
    , homeDirectory ? "/home/${username}"
    , pkgs ? builtins.getAttr system allPkgs
    , unstable ? builtins.getAttr system allUnstablePkgs
    , myPkgs ? builtins.getAttr system self.packages }:
    home-manager.lib.homeManagerConfiguration {
      inherit system homeDirectory username pkgs configuration;
      stateVersion = "22.05";
      extraModules = [ ./modules ];
      extraSpecialArgs = { inherit inputs myPkgs unstable system; };
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

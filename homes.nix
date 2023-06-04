{ inputs, allPkgs, allUnstablePkgs }:

let
  inherit (inputs) home-manager self;
  buildHomeManager = configuration:
    { system ? "x86_64-linux", username ? "adomas"
    , homeDirectory ? "/home/${username}"
    , pkgs ? builtins.getAttr system allPkgs
    , unstable ? builtins.getAttr system allUnstablePkgs
    , myPkgs ? builtins.getAttr system self.packages }:
    home-manager.lib.homeManagerConfiguration {
      inherit pkgs;
      modules = [
        ./modules
        inputs.hyprland.homeManagerModules.default
        {
          home = {
            inherit username homeDirectory;
            stateVersion = "22.05";
          };
        }
        configuration
      ];
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
  pc = buildHomeManager ./profiles/pc.nix { };
  "adomas@adomo-nixos" = home;
  "adomas@adomo-t14" = t14;
  "adomas@arch-vm" = foreign;
  "adomas@adomas-jatuzis-nixos" = work;
  "adomas@adomo-pc-nixos" = pc;
  "pi@raspberrypi-nixos" = pi;
  thinkpad-home = work-remote;
  thinkpad-work = work;
}

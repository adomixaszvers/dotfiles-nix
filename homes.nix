{ home-manager, pkgs, unstable, myPkgs, system, nixpkgs-config, inputs }:

let
  username = "adomas";
  homeDirectory = "/home/adomas";
  buildHomeManager = config:
    home-manager.lib.homeManagerConfiguration {
      inherit system homeDirectory username pkgs;
      stateVersion = "21.11";
      extraSpecialArgs = { inherit inputs myPkgs unstable; };
      configuration = {
        imports = [ inputs.nix-doom-emacs.hmModule config ];
        nixpkgs = { config = nixpkgs-config; };
      };
    };
in rec {
  work = buildHomeManager ./profiles/work.nix;
  work-remote = buildHomeManager ./profiles/work-remote.nix;
  home = buildHomeManager ./profiles/home.nix;
  foreign = buildHomeManager ./profiles/foreign.nix;
  "adomas@adomo-nixos" = home;
  "adomas@arch-vm" = foreign;
  "adomas@adomas-jatuzis-nixos" = work-remote;
}

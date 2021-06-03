{ home-manager, pkgs, myPkgs, system, overlays, nixpkgs-config, inputs }:

let
  username = "adomas";
  homeDirectory = "/home/adomas";
  buildHomeManager = config:
    home-manager.lib.homeManagerConfiguration {
      inherit system homeDirectory username pkgs;
      extraSpecialArgs = { inherit inputs myPkgs; };
      configuration = { ... }: {
        imports = [ inputs.nix-doom-emacs.hmModule config ];
        nixpkgs = {
          inherit overlays;
          config = nixpkgs-config;
        };
      };
    };
in rec {
  work = buildHomeManager ./profiles/work.nix;
  work-remote = buildHomeManager ./profiles/work-remote.nix;
  home = buildHomeManager ./profiles/home.nix;
  "adomas@adomo-nixos" = home;
  "adomas@adomas-jatuzis-nixos" = work-remote;
}

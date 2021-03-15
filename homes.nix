{ self, home-manager, pkgs, system, overlays, nixpkgs-config, inputs }:

let
  username = "adomas";
  homeDirectory = "/home/adomas";
  buildHomeManager = config: rec {
    hmModule = home-manager.lib.homeManagerConfiguration {
      inherit system homeDirectory username pkgs;
      extraSpecialArgs = { inherit inputs; };
      configuration = { ... }: {
        imports = [ inputs.nix-doom-emacs.hmModule config ];
        nixpkgs = {
          inherit overlays;
          config = nixpkgs-config;
        };
      };
    };
    activate = {
      type = "app";
      program = "${hmModule.activationPackage}/activate";
    };
  };
in {
  work = buildHomeManager "${self}/profiles/work.nix";
  work-remote = buildHomeManager "${self}/profiles/work-remote.nix";
  home = buildHomeManager "${self}/profiles/home.nix";
}

{ self, home-manager, pkgs, system, overlays, nixpkgs-config }:

let
  username = "adomas";
  homeDirectory = "/home/adomas";
  buildHomeManager = config: rec {
    hmModule = home-manager.lib.homeManagerConfiguration {
      inherit system homeDirectory username pkgs;
      configuration = { ... }: {
        imports = [ pkgs.nivSources.nix-doom-emacs.hmModule config ];
        nixpkgs = {
          inherit overlays;
          config = nixpkgs-config;
        };
      };
    };
    activate = pkgs.writeShellScriptBin "home-manager-activate" ''
      exec ${hmModule.activationPackage}/activate
    '';
  };
in {
  work = buildHomeManager "${self}/profiles/work.nix";
  home = buildHomeManager "${self}/profiles/home.nix";
}

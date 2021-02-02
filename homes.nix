{ self, home-manager, pkgs, system }:

let
  username = "adomas";
  homeDirectory = "/home/adomas";
  buildHomeManager = config: rec {
    hmModule = home-manager.lib.homeManagerConfiguration {
      inherit system homeDirectory username pkgs;
      configuration = { ... }: { imports = [ config ]; };
    };
    activate = pkgs.writeShellScriptBin "home-manager-activate" ''
      #!${pkgs.runtimeShell}
      exec ${hmModule.activationPackage}/activate
    '';
  };
in {
  work = buildHomeManager "${self}/profiles/work.nix";
  home = buildHomeManager "${self}/profiles/home.nix";
}

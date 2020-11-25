{ self, home-manager, pkgs }:

let
  extendedLib =
    import "${home-manager}/modules/lib/stdlib-extended.nix" pkgs.lib;
  buildHomeManager = configuration: rec {
    hmModules = import "${home-manager}/modules/modules.nix" {
      inherit pkgs;
      lib = extendedLib;
      useNixpkgsModule = false;
    };
    module =
      extendedLib.evalModules { modules = [ { } configuration ] ++ hmModules; };
    activate = pkgs.writeShellScriptBin "home-manager-activate" ''
      #!${pkgs.runtimeShell}
      exec ${module.config.home.activationPackage}/activate
    '';
  };
in {
  work = buildHomeManager "${self}/profiles/work.nix";
  home = buildHomeManager "${self}/profiles/home.nix";
}

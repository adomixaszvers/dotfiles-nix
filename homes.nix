{ self, nixpkgs, home-manager, system, sources, all-hies }:

let
  pkgs = nixpkgs.legacyPackages."${system}";
  extendedLib =
    import "${home-manager}/modules/lib/stdlib-extended.nix" pkgs.lib;
  overlays = let
    hie = "${all-hies}/overlay.nix";
    mine = _: super: { mine = self.packages."${super.system}"; };
    nivSources = _: _: { nivSources = sources; };
    gitignoreSource = _: super:
      let gitignore = (import sources.gitignore) { inherit (super) lib; };
      in { inherit (gitignore) gitignoreSource; };
    nixos-unstable = _: super: {
      nixos-unstable = sources.nixos-unstable.legacyPackages."${super.system}";
    };
  in [ mine hie nixos-unstable nivSources gitignoreSource ];
  buildHomeManager = configuration: rec {
    hmModules = import "${home-manager}/modules/modules.nix" {
      inherit pkgs;
      lib = extendedLib;
      useNixpkgsModule = true;
    };
    module = extendedLib.evalModules {
      modules = [ { nixpkgs.overlays = overlays; } configuration ] ++ hmModules;
    };
    activate = pkgs.writeShellScript "home-manager-activate" ''
      #!${pkgs.runtimeShell}
      exec ${module.config.home.activationPackage}/activate
    '';
  };
in {
  work = buildHomeManager "${self}/hosts/work.nix";
  home = buildHomeManager "${self}/hosts/home.nix";
}

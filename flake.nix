{
  description = "My dotfiles";
  inputs = {
    awesome-copycats = {
      url = "github:lcpz/awesome-copycats";
      flake = false;
    };
    awesome-sharedtags = {
      url = "github:Drauthius/awesome-sharedtags/v4.0";
      flake = false;
    };
    bumblebee-status = {
      url = "github:tobi-wan-kenobi/bumblebee-status/v2.0.5";
      flake = false;
    };
    chemacs = {
      url = "github:plexus/chemacs2";
      flake = false;
    };
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    flake-utils = { url = "github:numtide/flake-utils"; };
    fz = {
      url = "github:changyuheng/fz";
      flake = false;
    };
    gitignore = {
      url = "github:hercules-ci/gitignore";
      flake = false;
    };
    home-manager = {
      url = "github:nix-community/home-manager/release-21.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    kakoune-sudo-write = {
      url = "github:occivink/kakoune-sudo-write";
      flake = false;
    };
    kakoune-text-objects = {
      url = "github:Delapouite/kakoune-text-objects";
      flake = false;
    };
    nix-doom-emacs = { url = "github:vlaci/nix-doom-emacs"; };
    emacs-overlay = { url = "github:nix-community/emacs-overlay"; };
    nixos-hardware = { url = "github:NixOS/nixos-hardware"; };
    nixos-unstable = { url = "github:NixOS/nixpkgs/nixos-unstable-small"; };
    nixpkgs = { url = "github:NixOS/nixpkgs/nixos-21.05"; };
    pre-commit-hooks = { url = "github:cachix/pre-commit-hooks.nix"; };
  };
  outputs = { self, nixpkgs, bumblebee-status, flake-utils, home-manager
    , nixos-hardware, pre-commit-hooks, ... }@inputs:
    let
      config = import ./config.nix;
      mkPkgs = system:
        let
          sxhkd-with-lt-keys = _: prev: {
            sxhkd = prev.sxhkd.overrideAttrs
              (_: { patches = [ ./pkgs/sxhkd.patch ]; });
          };
          gitignoreSource = _: prev:
            let gitignore = (import inputs.gitignore) { inherit (prev) lib; };
            in { inherit (gitignore) gitignoreSource; };
          nixos-unstable = _: _: {
            nixos-unstable =
              import inputs.nixos-unstable { inherit system config; };
          };
        in rec {
          overlays = [ gitignoreSource nixos-unstable sxhkd-with-lt-keys ];
          pkgs = import nixpkgs { inherit system overlays config; };
        };
    in (flake-utils.lib.eachSystem [ "x86_64-linux" ] (system: rec {
      packages = import ./pkgs {
        pkgs = (mkPkgs system).pkgs;
        bumblebee-status-source = bumblebee-status;
      };
      checks = {
        pre-commit-check = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            nixfmt.enable = true;
            nix-linter.enable = true;
            shellcheck.enable = true;
          };
        };
      };
      devShell = nixpkgs.legacyPackages.${system}.mkShell {
        inherit (checks.pre-commit-check) shellHook;
      };
    })) // {
      nixosConfigurations =
        import ./nixos/configurations.nix { inherit nixpkgs nixos-hardware; };
      homeConfigurations = with (mkPkgs "x86_64-linux");
        import ./homes.nix {
          inherit home-manager pkgs system overlays inputs;
          nixpkgs-config = config;
          myPkgs = self.packages.x86_64-linux;
        };
    };
}

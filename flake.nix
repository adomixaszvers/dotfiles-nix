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
    flake-utils = { url = "github:numtide/flake-utils"; };
    fz = {
      url = "github:changyuheng/fz";
      flake = false;
    };
    gitignore = {
      url = "github:hercules-ci/gitignore";
      flake = false;
    };
    kakoune-sudo-write = {
      url = "github:occivink/kakoune-sudo-write";
      flake = false;
    };
    kakoune-text-objects = {
      url = "github:Delapouite/kakoune-text-objects";
      flake = false;
    };
    nix-doom-emacs = {
      url = "github:vlaci/nix-doom-emacs";
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };
    home-manager = {
      url = "github:nix-community/home-manager/release-20.09";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    "pre-commit-hooks.nix" = {
      url = "github:cachix/pre-commit-hooks.nix";
      flake = false;
    };
    nixpkgs = { url = "github:NixOS/nixpkgs/nixos-20.09"; };
    nixos-unstable = { url = "github:NixOS/nixpkgs/nixos-unstable"; };
    nixos-hardware = { url = "github:NixOS/nixos-hardware"; };
  };
  outputs = { self, nixpkgs, bumblebee-status, flake-utils, home-manager
    , nixos-hardware, ... }@sources:
    (flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        config = import ./config.nix;
        overlays = let
          mine = _: _: { mine = self.packages."${system}"; };
          sxhkd-with-lt-keys = _: prev: {
            sxhkd = prev.sxhkd.overrideAttrs
              (_: { patches = [ ./pkgs/sxhkd.patch ]; });
          };
          nivSources = _: _: { nivSources = sources; };
          gitignoreSource = _: prev:
            let gitignore = (import sources.gitignore) { inherit (prev) lib; };
            in { inherit (gitignore) gitignoreSource; };
          nixos-unstable = _: _: {
            nixos-unstable = import sources.nixos-unstable {
              inherit system config;
              overlays = [ mine ];
            };
          };
        in [
          gitignoreSource
          mine
          nivSources
          nixos-unstable
          sxhkd-with-lt-keys
        ];
        pkgs = import nixpkgs { inherit system overlays config; };
      in {
        apps = {
          hm-home = self.homes."${system}".home.activate;
          hm-work = self.homes."${system}".work.activate;
        };
        packages = import ./pkgs {
          inherit pkgs;
          bumblebee-status-source = bumblebee-status;
        };
        homes = import ./homes.nix {
          inherit self home-manager pkgs system overlays;
          nixpkgs-config = config;
        };
      })) // {
        nixosConfigurations =
          import ./nixos/configurations.nix { inherit nixpkgs nixos-hardware; };
      };
}

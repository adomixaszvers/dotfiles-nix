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
      url = "github:nix-community/home-manager/release-20.09";
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
    nix-doom-emacs = {
      url = "github:vlaci/nix-doom-emacs";
      inputs = {
        emacs-overlay.follows = "emacs-overlay";
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };
    emacs-overlay = { url = "github:nix-community/emacs-overlay"; };
    nixos-hardware = { url = "github:NixOS/nixos-hardware"; };
    nixos-unstable = { url = "github:NixOS/nixpkgs/nixos-unstable"; };
    nixpkgs = { url = "github:NixOS/nixpkgs/nixos-20.09"; };
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      flake = false;
    };
  };
  outputs = { nixpkgs, bumblebee-status, flake-utils, home-manager
    , nixos-hardware, ... }@inputs:
    (flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        config = import ./config.nix;
        overlays = let
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
        in [ gitignoreSource nixos-unstable sxhkd-with-lt-keys ];
        pkgs = import nixpkgs { inherit system overlays config; };
      in rec {
        apps = {
          hm-home = homes.home.activate;
          hm-work = homes.work.activate;
        };
        packages = import ./pkgs {
          inherit pkgs;
          bumblebee-status-source = bumblebee-status;
        };
        homes = import ./homes.nix {
          inherit home-manager pkgs system overlays inputs;
          nixpkgs-config = config;
          myPkgs = packages;
        };
      })) // {
        nixosConfigurations =
          import ./nixos/configurations.nix { inherit nixpkgs nixos-hardware; };
      };
}

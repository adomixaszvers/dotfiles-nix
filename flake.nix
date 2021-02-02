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
    nix-doom-emacs = { url = "github:vlaci/nix-doom-emacs"; };
    home-manager = { url = "github:rycee/home-manager/release-20.09"; };
    nixpkgs = { url = "github:NixOS/nixpkgs/nixos-20.09"; };
    nixos-unstable = { url = "github:NixOS/nixpkgs/nixos-unstable"; };
  };
  outputs =
    { self, nixpkgs, bumblebee-status, flake-utils, home-manager, ... }@sources:
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
        packages = import ./pkgs {
          inherit pkgs;
          bumblebee-status-source = bumblebee-status;
        } // {
          hm-home = self.homes."${system}".home.activate;
          hm-work = self.homes."${system}".work.activate;
        };
        homes = import ./homes.nix {
          inherit self home-manager pkgs system overlays;
          nixpkgs-config = config;
        };
      }));
}

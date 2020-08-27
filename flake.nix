{
  description = "My dotfiles";
  inputs = {
    all-hies = {
      url = "github:Infinisil/all-hies";
      flake = false;
    };
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
      flake = false;
    };
    home-manager = {
      url = "github:rycee/home-manager";
      flake = false;
    };
    nixpkgs = { url = "github:NixOS/nixpkgs-channels/nixos-20.03"; };
    nixos-unstable = { url = "github:NixOS/nixpkgs-channels/nixos-unstable"; };
  };
  outputs = { self, nixpkgs, bumblebee-status, flake-utils, all-hies
    , home-manager, ... }@sources:
    (flake-utils.lib.eachDefaultSystem (system: {
      packages = import "${self}/pkgs" system {
        inherit nixpkgs;
        bumblebee-status-source = bumblebee-status;
      };
      homes = import "${self}/homes.nix" {
        inherit self nixpkgs home-manager system sources all-hies;
      };
    })) // {

    };
}

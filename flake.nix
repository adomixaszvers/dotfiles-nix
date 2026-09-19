{
  description = "My dotfiles";
  inputs = {
    flake-compat.url = "github:edolstra/flake-compat";
    flake-parts.url = "github:hercules-ci/flake-parts";
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixGL = {
      url = "github:nix-community/nixGL";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };
    nix-index-database = {
      url = "github:nix-community/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-wrapper-modules = {
      url = "github:BirdeeHub/nix-wrapper-modules";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixbot = {
      url = "github:Mic92/nixbot";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        treefmt-nix.follows = "treefmt";
      };
    };
    nixos-hardware = {
      url = "github:NixOS/nixos-hardware";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-unstable.follows = "nixpkgs";
    # nixpkgs.url = "https://channels.nixos.org/nixos-unstable/nixexprs.tar.xz";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };
    stylix = {
      url = "github:nix-community/stylix/master";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-parts.follows = "flake-parts";
      };
    };
    treefmt = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs =
    {
      nixpkgs,
      nixos-unstable,
      flake-parts,
      treefmt,
      ...
    }@inputs:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        # "aarch64-linux"
      ];
      imports = [
        treefmt.flakeModule
        ./wrappers
        ./modules/nixbotJobs.nix
        ./pkgs
        ./devshells.nix
        ./nixos
        ./home-manager
        ./nixbotJobs.nix
      ];
      perSystem =
        {
          system,
          ...
        }:
        {
          _module.args = {
            pkgs = import nixpkgs {
              inherit system;
              config = import ./config.nix;
            };
            unstable = import nixos-unstable {
              inherit system;
              config = import ./config.nix;
            };
          };
          treefmt = {
            programs = {
              keep-sorted.enable = true;
              nixfmt.enable = true;
              statix = {
                enable = true;
                excludes = [ "hardware-configuration.nix" ];
              };
              stylua = {
                enable = true;
                settings = {
                  # like in https://github.com/neovim/neovim/blob/5d3df1c41afec9e0dd4609ae3dfbccbf3ccd3241/.stylua.toml
                  column_width = 100;
                  line_endings = "Unix";
                  indent_type = "Spaces";
                  indent_width = 2;
                  quote_style = "AutoPreferSingle";
                  call_parentheses = "Input";
                };
              };
              deadnix.enable = true;
              shellcheck.enable = true;
            };
          };
        };
    };
}

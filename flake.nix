{
  description = "My dotfiles";
  inputs = {
    flake-compat.url = "github:edolstra/flake-compat";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-utils.url = "github:numtide/flake-utils";
    home-manager = {
      url = "github:nix-community/home-manager/release-24.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixGL = {
      url = "github:guibou/nixGL";
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };
    nix-index-database = {
      url = "github:nix-community/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    nixos-unstable.url = "github:NixOS/nixpkgs/nixos-unstable-small";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };
    stylix = {
      # TODO fix the reference when 24.11 released
      url = "github:danth/stylix/master";
      inputs = {
        flake-compat.follows = "flake-compat";
        home-manager.follows = "home-manager";
        nixpkgs.follows = "nixpkgs";
      };
    };
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs = {
        flake-compat.follows = "flake-compat";
        nixpkgs.follows = "nixpkgs";
        nixpkgs-stable.follows = "";
      };
    };
  };
  outputs =
    {
      nixpkgs,
      pre-commit-hooks,
      nixos-unstable,
      flake-parts,
      ...
    }@inputs:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];
      imports = [
        pre-commit-hooks.flakeModule
        ./pkgs
        ./nixos/configurations.nix
        ./homes.nix
      ];
      perSystem =
        {
          pkgs,
          system,
          self',
          inputs',
          config,
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
          apps.my-neovim =
            let
              myNeovim = self'.packages.neovim;
            in
            {
              type = "app";
              program = "${myNeovim}/bin/nvim";
            };
          pre-commit.settings = {
            src = ./.;
            hooks = {
              nixfmt-rfc-style.enable = true;
              statix = {
                enable = true;
                settings.ignore = [ "hardware-configuration.nix" ];
              };
              deadnix.enable = true;
              shellcheck = {
                enable = true;
                # shellcheck does not support zsh files but after
                # https://github.com/cachix/pre-commit-hooks.nix/commit/61bda56530889b4acf6c935d832f219b6c0ebd83
                # it is run on initExtra.zsh and it fails on pre-commit
                excludes = [ "\\.zsh$" ];
              };
            };
          };
          devShells = {
            default = pkgs.mkShellNoCC {
              buildInputs = [
                inputs'.home-manager.packages.home-manager
                pkgs.sops
              ] ++ config.pre-commit.settings.enabledPackages;
              shellHook = config.pre-commit.installationScript;
            };
            xmonad = import ./profiles/wm/xmonad/shell.nix { inherit pkgs; };
            qtile = import ./profiles/wm/qtile/shell.nix { inherit pkgs; };
            awesomewm = import ./profiles/wm/awesome/shell.nix { inherit pkgs; };
          };
        };
    };
}

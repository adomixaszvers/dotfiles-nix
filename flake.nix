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
    flake-parts.url = "github:hercules-ci/flake-parts";
    fz = {
      url = "github:changyuheng/fz";
      flake = false;
    };
    home-manager = {
      url = "github:nix-community/home-manager/release-23.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-colors.url = "github:misterio77/nix-colors";
    kakoune-sudo-write = {
      url = "github:occivink/kakoune-sudo-write";
      flake = false;
    };
    kakoune-text-objects = {
      url = "github:Delapouite/kakoune-text-objects";
      flake = false;
    };
    nixGL = {
      url = "github:guibou/nixGL";
      inputs = { nixpkgs.follows = "nixpkgs"; };
    };
    nix-index-database = {
      url = "github:nix-community/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    nixos-unstable.url = "github:NixOS/nixpkgs/nixos-unstable-small";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    pre-commit-hooks = { url = "github:cachix/pre-commit-hooks.nix"; };
  };
  outputs =
    { nixpkgs, pre-commit-hooks, nixos-unstable, flake-parts, ... }@inputs:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-linux" ];
      imports = [
        pre-commit-hooks.flakeModule
        ./pkgs
        ./nixos/configurations.nix
        ./homes.nix
      ];
      perSystem = { pkgs, system, self', inputs', config, ... }: {
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
        apps.my-neovim = let myNeovim = self'.packages.neovim;
        in {
          type = "app";
          program = "${myNeovim}/bin/nvim";
        };
        pre-commit.settings = {
          src = ./.;
          hooks = {
            nixfmt.enable = true;
            statix.enable = true;
            deadnix.enable = true;
            shellcheck = {
              enable = true;
              # shellcheck does not support zsh files but after
              # https://github.com/cachix/pre-commit-hooks.nix/commit/61bda56530889b4acf6c935d832f219b6c0ebd83
              # it is run on initExtra.zsh and it fails on pre-commit
              excludes = [ "\\.zsh$" ];
            };
          };
          settings.statix.ignore = [ "hardware-configuration.nix" ];
        };
        devShells = {
          default = pkgs.mkShell {
            buildInputs =
              [ inputs'.home-manager.packages.home-manager pkgs.sops ];
            shellHook =
              let precommitShellHook = config.pre-commit.installationScript;
              in ''
                if [ -f ./flake.nix ]; then
                  ${precommitShellHook}
                fi
              '';
          };
          xmonad = import ./profiles/wm/xmonad/shell.nix { inherit pkgs; };
          qtile = import ./profiles/wm/qtile/shell.nix { inherit pkgs; };
          awesomewm =
            import ./profiles/wm/awesome/shell.nix { inherit pkgs system; };
        };
      };
    };
}

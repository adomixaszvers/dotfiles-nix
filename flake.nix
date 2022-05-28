{
  description = "My dotfiles";
  inputs = {
    ani-cli = {
      url = "github:pystardust/ani-cli";
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
    chemacs = {
      url = "github:plexus/chemacs2";
      flake = false;
    };
    comma = {
      url = "github:nix-community/comma";
      inputs = {
        nixpkgs.follows = "nixos-unstable";
        utils.follows = "flake-utils";
        flake-compat.follows = "flake-compat";
      };
    };
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    fz = {
      url = "github:changyuheng/fz";
      flake = false;
    };
    gitignore = {
      url = "github:hercules-ci/gitignore";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:adomixaszvers/home-manager/release-21.11";
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
    flake-utils.url = "github:numtide/flake-utils";
    nixos-hardware = { url = "github:NixOS/nixos-hardware"; };
    nixos-2009 = { url = "github:NixOS/nixpkgs/nixos-20.09"; };
    nixos-unstable = { url = "github:NixOS/nixpkgs/nixos-unstable-small"; };
    nixpkgs = { url = "github:NixOS/nixpkgs/nixos-21.11"; };
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.flake-utils.follows = "flake-utils";
    };
  };
  outputs = { self, nixpkgs, home-manager, pre-commit-hooks, gitignore
    , flake-utils, ... }@inputs:
    let config = import ./config.nix;
    in flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-linux" ] (system:
      let
        pkgs = import nixpkgs { inherit system config; };
        unstable = import inputs.nixos-unstable { inherit system config; };
      in {
        apps.my-neovim =
          let myNeovim = (builtins.getAttr system self.packages).neovim;
          in {
            type = "app";
            program = "${myNeovim}/bin/nvim";
          };
        packages = import ./pkgs { inherit pkgs unstable system inputs; };
        checks = {
          pre-commit-check = pre-commit-hooks.lib."${system}".run {
            src = let
              inherit (gitignore.lib) gitignoreFilter;
              inherit (nixpkgs.lib) hasSuffix cleanSourceWith;
              currentDir = ./.;
              sourceIgnored = gitignoreFilter currentDir;
              customFilter = path: type:
                sourceIgnored path type
                && !builtins.any (sufix: hasSuffix sufix path) [
                  "pkgs/lua-fmt/default.nix"
                  "pkgs/lua-fmt/node-env.nix"
                  "pkgs/lua-fmt/node-packages.nix"
                  "pkgs/vimgolf/gemset.nix"
                  "profiles/wm/penrose/my-penrose-config/Cargo.nix"
                ];
            in cleanSourceWith {
              filter = customFilter;
              src = currentDir;
              name = "my-home-manager-config-source";
            };
            hooks = {
              nixfmt.enable = true;
              nix-linter.enable = true;
              shellcheck.enable = true;
            };
          };
        };
        devShells = {
          default = pkgs.mkShell {
            buildInputs =
              [ home-manager.packages."${system}".home-manager pkgs.sops ];
            inherit (self.checks."${system}".pre-commit-check) shellHook;
          };
          xmonad = import ./profiles/wm/xmonad/shell.nix { pkgs = unstable; };
          my-penrose =
            import ./profiles/wm/penrose/my-penrose-config/shell.nix {
              inherit pkgs;
            };
          qtile = import ./profiles/wm/qtile/shell.nix { inherit pkgs; };
          awesomewm = import ./profiles/wm/awesome/shell.nix {
            inherit pkgs system;
            mine = self;
          };
        };
      }) // {
        nixosConfigurations =
          import ./nixos/configurations.nix { inherit inputs; };
        homeConfigurations = import ./homes.nix { inherit inputs; };
      };
}

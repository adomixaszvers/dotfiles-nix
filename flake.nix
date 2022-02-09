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
      flake = false;
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
    nix-doom-emacs = {
      url = "github:nix-community/nix-doom-emacs";
      inputs.flake-utils.follows = "flake-utils";
    };
    emacs-overlay = { url = "github:nix-community/emacs-overlay"; };
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
        apps.my-neovim = let
          inherit ((home-manager.lib.homeManagerConfiguration {
            pkgs = unstable;
            inherit system;
            extraSpecialArgs = { inherit unstable; };
            username = "nobody";
            homeDirectory = "/dev/null";
            configuration = { imports = [ ./profiles/cli/neovim ]; };
          }).config.programs.neovim)
            finalPackage generatedConfigViml;
          initNvim = unstable.writeText "init.nvim" generatedConfigViml;
          wrapper = unstable.writeShellScript "my-neovim-wrapped" ''
            exec ${finalPackage}/bin/nvim -u ${initNvim} "$@"
          '';
        in {
          type = "app";
          program = "${wrapper}";
        };
        packages = import ./pkgs { inherit pkgs system inputs; };
        checks = let
          inherit (nixpkgs) lib;
          hmChecks = lib.mapAttrs' (name: value:
            lib.nameValuePair ("hm-check-" + name)
            (pkgs.writeText ("hm-check-" + name)
              (builtins.unsafeDiscardStringContext
                value.activationPackage.drvPath))) self.homeConfigurations;
        in {
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
        } // hmChecks;
        devShell = pkgs.mkShell {
          buildInputs =
            [ home-manager.packages."${system}".home-manager pkgs.sops ];
          inherit (self.checks."${system}".pre-commit-check) shellHook;
        };
      }) // {
        nixosConfigurations =
          import ./nixos/configurations.nix { inherit inputs; };
        homeConfigurations = import ./homes.nix { inherit inputs; };
      };
}

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
    credentials = {
      url = "git+file:///home/adomas/credentials-nix";
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
    nix-doom-emacs = { url = "github:vlaci/nix-doom-emacs"; };
    emacs-overlay = { url = "github:nix-community/emacs-overlay"; };
    nixos-hardware = { url = "github:NixOS/nixos-hardware"; };
    nixos-2009 = { url = "github:NixOS/nixpkgs/nixos-20.09"; };
    nixos-unstable = { url = "github:NixOS/nixpkgs/nixos-unstable-small"; };
    nixpkgs = { url = "github:NixOS/nixpkgs/nixos-21.11"; };
    pre-commit-hooks = { url = "github:cachix/pre-commit-hooks.nix"; };
  };
  outputs = { self, nixpkgs, ani-cli, bumblebee-status, flake-utils
    , home-manager, nixos-hardware, pre-commit-hooks, gitignore, ... }@inputs:
    let
      config = import ./config.nix;
      mkPkgs = system:
        let
          gitignoreSource = _: _: { inherit (gitignore.lib) gitignoreSource; };
          nixos-unstable = _: _: {
            nixos-unstable =
              import inputs.nixos-unstable { inherit system config; };
          };
        in rec {
          overlays = [ gitignoreSource nixos-unstable ];
          pkgs = import nixpkgs { inherit system overlays config; };
        };
    in (flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let pkgs = (mkPkgs system).pkgs;
      in rec {
        packages = import ./pkgs {
          inherit pkgs;
          ani-cli-source = ani-cli;
          bumblebee-status-source = bumblebee-status;
          home-manager = home-manager.packages.${system}.home-manager;
        };
        checks = {
          pre-commit-check = pre-commit-hooks.lib.${system}.run {
            src = builtins.filterSource (path: _type:
              !builtins.any (sufix: nixpkgs.lib.hasSuffix sufix path) [
                "pkgs/lua-fmt/default.nix"
                "pkgs/lua-fmt/node-env.nix"
                "pkgs/lua-fmt/node-packages.nix"
                "pkgs/vimgolf/gemset.nix"
                "profiles/wm/penrose/my-penrose-config/Cargo.nix"
              ]) ./.;
            hooks = {
              nixfmt.enable = true;
              nix-linter.enable = true;
              shellcheck.enable = true;
            };
          };
        };
        devShell = nixpkgs.legacyPackages.${system}.mkShell {
          buildInputs =
            [ packages.hm-switch home-manager.packages.${system}.home-manager ];
          inherit (checks.pre-commit-check) shellHook;
        };
      })) // {
        nixosConfigurations = import ./nixos/configurations.nix {
          inherit nixpkgs nixos-hardware inputs;
        };
        homeConfigurations = with (mkPkgs "x86_64-linux");
          import ./homes.nix {
            inherit home-manager pkgs system overlays inputs;
            nixpkgs-config = config;
            myPkgs = self.packages.x86_64-linux;
          };
      };
}

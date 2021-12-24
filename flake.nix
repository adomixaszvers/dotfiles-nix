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
    nix-doom-emacs = { url = "github:nix-community/nix-doom-emacs"; };
    emacs-overlay = { url = "github:nix-community/emacs-overlay"; };
    nixos-hardware = { url = "github:NixOS/nixos-hardware"; };
    nixos-2009 = { url = "github:NixOS/nixpkgs/nixos-20.09"; };
    nixos-unstable = { url = "github:NixOS/nixpkgs/nixos-unstable-small"; };
    nixpkgs = { url = "github:NixOS/nixpkgs/nixos-21.11"; };
    pre-commit-hooks = { url = "github:cachix/pre-commit-hooks.nix"; };
  };
  outputs = { self, nixpkgs, ani-cli, bumblebee-status, home-manager
    , nixos-hardware, pre-commit-hooks, gitignore, ... }@inputs:
    let
      config = import ./config.nix;
      system = "x86_64-linux";
      nixos-unstable-overlay = _: _: {
        nixos-unstable =
          import inputs.nixos-unstable { inherit system config; };
      };
      overlays = [ nixos-unstable-overlay ];
      pkgs = import nixpkgs { inherit system overlays config; };
    in {
      packages."${system}" = import ./pkgs {
        inherit pkgs;
        inherit (home-manager.packages."${system}") home-manager;
        ani-cli-source = ani-cli;
        bumblebee-status-source = bumblebee-status;
      };
      checks."${system}" = {
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
      devShell."${system}" = pkgs.mkShell {
        buildInputs = [ home-manager.packages."${system}".home-manager ];
        inherit (self.checks."${system}".pre-commit-check) shellHook;
      };
      nixosConfigurations = import ./nixos/configurations.nix {
        inherit nixpkgs nixos-hardware inputs;
      };
      homeConfigurations = import ./homes.nix {
        inherit home-manager pkgs system overlays inputs;
        nixpkgs-config = config;
        myPkgs = self.packages.x86_64-linux;
      };
    };
}

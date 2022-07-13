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
    emacs-overlay.url = "github:nix-community/emacs-overlay";
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
      url = "github:nix-community/home-manager/release-22.05";
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
    nixpkgs = { url = "github:NixOS/nixpkgs/nixos-22.05"; };
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
    , flake-utils, nixos-unstable, ... }@inputs:
    let
      config = import ./config.nix;
      systems = [ "x86_64-linux" "aarch64-linux" ];
      mkPkgs = nixpkgs: system: import nixpkgs { inherit system config; };
      allPkgs = nixpkgs.lib.genAttrs systems (mkPkgs nixpkgs);
      allUnstablePkgs = nixpkgs.lib.genAttrs systems (mkPkgs nixos-unstable);
    in flake-utils.lib.eachSystem systems (system:
      let
        pkgs = builtins.getAttr system allPkgs;
        # unstable = import inputs.nixos-unstable { inherit system config; };
      in {
        apps.my-neovim =
          let myNeovim = (builtins.getAttr system self.packages).neovim;
          in {
            type = "app";
            program = "${myNeovim}/bin/nvim";
          };
        packages = import ./pkgs { inherit pkgs system inputs; };
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
            shellHook = let
              precommitShellHook =
                self.checks."${system}".pre-commit-check.shellHook;
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
      }) // {
        nixosConfigurations =
          import ./nixos/configurations.nix { inherit inputs; };
        homeConfigurations =
          import ./homes.nix { inherit inputs allPkgs allUnstablePkgs; };
      };
}

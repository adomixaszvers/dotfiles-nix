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
    fz = {
      url = "github:changyuheng/fz";
      flake = false;
    };
    home-manager = {
      url = "github:nix-community/home-manager/release-23.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    hyprland.url = "github:hyprwm/Hyprland";
    hyprland-contrib.url = "github:hyprwm/contrib";
    kakoune-sudo-write = {
      url = "github:occivink/kakoune-sudo-write";
      flake = false;
    };
    kakoune-text-objects = {
      url = "github:Delapouite/kakoune-text-objects";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
    nixGL = {
      url = "github:guibou/nixGL";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
    nixos-hardware = { url = "github:NixOS/nixos-hardware"; };
    nixos-unstable = { url = "github:NixOS/nixpkgs/nixos-unstable-small"; };
    nixpkgs = { url = "github:NixOS/nixpkgs/nixos-23.05"; };
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.flake-utils.follows = "flake-utils";
    };
  };
  outputs = { self, nixpkgs, home-manager, pre-commit-hooks, flake-utils
    , nixos-unstable, ... }@inputs:
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
            src = ./.;
            hooks = {
              nixfmt.enable = true;
              statix.enable = true;
              deadnix.enable = true;
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

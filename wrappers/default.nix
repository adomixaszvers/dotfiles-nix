{ inputs, ... }:
{
  imports = [
    inputs.nix-wrapper-modules.flakeModules.wrappers
  ];

  perSystem =
    {
      system,
      ...
    }:
    let
      isArm = system == "aarch64-linux";
    in
    {
      wrappers = {
        # wrappers.pkgs = pkgs; # choose a different `pkgs`
        control_type = "exclude"; # | "build" (default: "exclude")
        packages = {
          neovim = isArm; # <- set to true to exclude from being built into `packages.*.*` flake output
          niri = isArm;
        };
      };
    };
  flake.wrappers = {

    neovim = {
      imports = [
        ./neovim/default.nix
        ./neovim/general.nix
        ./neovim/extra.nix
        ./neovim/treesitter-full.nix
      ];
    };
    neovim-nix = {
      imports = [
        ./neovim/default.nix
        ./neovim/general.nix
        ./neovim/treesitter-small.nix
      ];
    };
    niri = {
      imports = [ ((import ./niri) { inherit inputs; }) ];
    };
  };
}

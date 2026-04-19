{ inputs, ... }:
{
  imports = [
    inputs.nix-wrapper-modules.flakeModules.wrappers
  ];

  perSystem =
    { system, ... }:
    {
      # wrappers.pkgs = pkgs; # choose a different `pkgs`
      wrappers.control_type = "exclude"; # | "build" (default: "exclude")
      wrappers.packages = {
        neovim = system == "aarch64-linux"; # <- set to true to exclude from being built into `packages.*.*` flake output
      };
    };

  flake.wrappers.neovim = {
    imports = [
      ./neovim/default.nix
      ./neovim/general.nix
      ./neovim/extra.nix
      ./neovim/treesitter-full.nix
    ];
  };
  flake.wrappers.neovim-nix = {
    imports = [
      ./neovim/default.nix
      ./neovim/general.nix
      ./neovim/treesitter-small.nix
    ];
  };
}

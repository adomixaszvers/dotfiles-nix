let
  flake = builtins.getFlake ("git+file://" + toString ./.);
  nixpkgs = import flake.inputs.nixpkgs { };
in {
  inherit flake;
} // flake // builtins // nixpkgs // nixpkgs.lib // flake.nixosConfigurations

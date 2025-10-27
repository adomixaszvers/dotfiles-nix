let
  flake = builtins.getFlake ("git+file://" + toString ./.);
  inherit (flake.inputs) nixpkgs;
  pkgs = import flake.inputs.nixpkgs { };
  unstable = import flake.inputs.nixos-unstable { };
in
flake
// {
  inherit flake pkgs unstable;
  inherit (nixpkgs) lib;
}

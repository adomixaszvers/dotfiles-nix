let
  flake = builtins.getFlake ("git+file://" + toString ./.);
  inherit (flake.inputs) nixpkgs;
  pkgs = import flake.inputs.nixpkgs { };
in
flake
// {
  inherit flake pkgs;
  inherit (nixpkgs) lib;
}

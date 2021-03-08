let
  inputs = import ./nix/sources.nix;
  home-manager =
    (import inputs.home-manager) { pkgs = import inputs.nixpkgs { }; };
in home-manager.install

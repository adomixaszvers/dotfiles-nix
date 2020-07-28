let
  nivSources = import ./nix/sources.nix;
  home-manager =
    (import nivSources.home-manager) { pkgs = import nivSources.nixpkgs { }; };
in home-manager.install

let
  nivSources = import ./nix/sources.nix;
  pkgs = import nivSources.nixpkgs { };
in pkgs.mkShell {
  shellHook = ''
    ${(import ./default.nix).pre-commit-check.shellHook}
  '';
}

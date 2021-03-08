let
  inputs = import ./nix/sources.nix;
  pkgs = import inputs.nixpkgs { };
in pkgs.mkShell {
  buildInputs = with pkgs; [ git-crypt ];
  shellHook = ''
    ${(import ./default.nix).pre-commit-check.shellHook}
  '';
}

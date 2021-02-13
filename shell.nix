let
  self = builtins.getFlake (toString ./.);
  pkgs = import self.inputs.nixpkgs { };
in pkgs.mkShell {
  shellHook = ''
    ${(import ./default.nix).pre-commit-check.shellHook}
  '';
}

let
  self = builtins.getFlake (toString ./.);
  pkgs = import self.inputs.nixpkgs { };
in pkgs.mkShell {
  buildInputs = with pkgs; [ git-crypt ];
  shellHook = ''
    ${(import ./default.nix).pre-commit-check.shellHook}
  '';
}

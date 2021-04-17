let
  self = builtins.getFlake (toString ./.);
  system = builtins.currentSystem;
  pkgs = import self.inputs.nixpkgs { };
in pkgs.mkShell {
  buildInputs = (with pkgs; [ git-crypt ])
    ++ (with self.packages."${system}"; [ hm-switch ]);
  shellHook = ''
    ${(import ./default.nix).pre-commit-check.shellHook}
  '';
}

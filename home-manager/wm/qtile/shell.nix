{
  pkgs ? import <nixpkgs> { },
}:
pkgs.mkShellNoCC {
  name = "qtile-shell";
  packages = [
    (pkgs.python3.withPackages (
      ps:
      builtins.attrValues {
        inherit (ps)
          # keep-sorted start
          python-lsp-black
          python-lsp-server
          qtile
          # keep-sorted end
          ;
      }
    ))
  ];
}

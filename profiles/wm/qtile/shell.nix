{
  pkgs ? import <nixpkgs> { },
}:
pkgs.mkShellNoCC {
  name = "qtile-shell";
  packages = [
    (pkgs.python3.withPackages (
      ps: with ps; [
        python-lsp-server
        python-lsp-black
        qtile
      ]
    ))
  ];
}

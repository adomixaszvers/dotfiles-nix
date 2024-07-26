{
  pkgs ? import <nixpkgs> { },
}:
pkgs.mkShellNoCC {
  name = "qtile-shell";
  buildInputs = [
    (pkgs.python3.withPackages (
      ps: with ps; [
        python-lsp-server
        python-lsp-black
        qtile
      ]
    ))
  ];
}

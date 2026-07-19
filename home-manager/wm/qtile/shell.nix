{
  pkgs ? import <nixpkgs> { },
}:
pkgs.mkShellNoCC {
  name = "qtile-shell";
  packages = [
    (pkgs.python3.withPackages (
      ps: with ps; [
        # keep-sorted start
        python-lsp-black
        python-lsp-server
        qtile
        # keep-sorted end
      ]
    ))
  ];
}

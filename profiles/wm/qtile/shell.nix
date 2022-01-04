{ pkgs ?
  (builtins.getFlake "nixpkgs").legacyPackages."${builtins.currentSystem}" }:
pkgs.mkShell {
  name = "qtile-shell";
  buildInputs = [
    (pkgs.python3.withPackages (ps:
      with ps; [
        xlib
        python-lsp-server
        python-lsp-black
        pkgs.qtile.unwrapped
      ]))
  ];
}

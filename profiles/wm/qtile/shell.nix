{ pkgs ?
  (builtins.getFlake "nixpkgs").legacyPackages."${builtins.currentSystem}" }:
let qtile = import ./myQtile.nix { inherit pkgs; };
in pkgs.mkShell {
  name = "qtile-shell";
  buildInputs = [
    (pkgs.python3.withPackages (ps:
      with ps; [
        xlib
        python-lsp-server
        python-lsp-black
        qtile.unwrapped
      ]))
  ];
}

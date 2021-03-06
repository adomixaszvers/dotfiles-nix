{ pkgs ?
  (builtins.getFlake "nixpkgs").legacyPackages."${builtins.currentSystem}" }:
with pkgs;
mkShell {
  name = "qtile-shell";
  buildInputs = with pkgs.python3Packages; [
    qtile
    black
    python-language-server
    xlib
  ];
}

{ pkgs, inputs, ... }: {
  home.packages =
    let
      bravePkgs = builtins.getAttr pkgs.stdenv.hostPlatform.system inputs.brave.packages;
      inherit (bravePkgs) brave;
    in
    [ brave ];
}

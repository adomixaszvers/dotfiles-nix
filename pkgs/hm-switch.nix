{ writeShellScriptBin }:
writeShellScriptBin "hm-switch" ''
  exec home-manager switch --flake ~/.config/nixpkgs
''

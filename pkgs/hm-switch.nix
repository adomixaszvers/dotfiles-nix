{ writeShellScriptBin, home-manager }:
writeShellScriptBin "hm-switch" ''
  exec ${home-manager}/bin/home-manager switch --flake ~/.config/nixpkgs
''

{ writeShellScriptBin }:
writeShellScriptBin "hm-switch" ''
  exec nix run -f ~/.config/nixpkgs/current.nix
''

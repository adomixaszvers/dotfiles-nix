{ writeShellScriptBin }:
writeShellScriptBin "hm-repl" ''
  exec nix repl ~/.config/nixpkgs/repl.nix
''

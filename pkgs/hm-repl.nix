{ writeShellScriptBin }:
writeShellScriptBin "hm-repl" ''
  exec nix repl --file ~/.config/nixpkgs/repl.nix
''

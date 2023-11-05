{ writeShellScriptBin, fzf }:
writeShellScriptBin "he" ''
  cd ~/.config/nixpkgs || exit 1
  readarray -t files < <(${fzf}/bin/fzf --multi)
  if [ -n "''${files[0]}" ]; then
    exec $EDITOR "''${files[@]}"
  fi
''

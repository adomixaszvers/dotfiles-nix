{
  lib,
  fd,
  fzf,
  writeShellScriptBin,
}:
writeShellScriptBin "he" ''
  cd ~/.config/nixpkgs || exit 1
  readarray -t files < <(${lib.getExe fd} --hidden --exclude=.git| ${lib.getExe fzf} --multi)
  if [ -n "''${files[0]}" ]; then
    exec $EDITOR "''${files[@]}"
  fi
''

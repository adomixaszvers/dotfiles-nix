{ writeShellScriptBin, gnused }:
writeShellScriptBin "kaknix" ''
  if [ $# -ne 1 ] || [ ! -f "$1"  ]; then
    exit 1
  fi
  nix-instantiate --parse "$1" 2>&1 >&- | ${gnused}/bin/sed 's/^\(.\+\), at \(.\+\)$/\2: \1/'
''

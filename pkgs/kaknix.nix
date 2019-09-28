{ runtimeShell, writeScriptBin, nix, gnused, lib }:
writeScriptBin "kaknix" ''
  #!${runtimeShell}
  PATH=${lib.makeBinPath [ nix gnused ]}:$PATH
  if [ $# -ne 1 ] || [ ! -f "$1"  ]; then
    exit 1
  fi
  nix-instantiate --parse "$1" 2>&1 >&- | sed 's/^\(.\+\), at \(.\+\)$/\2: \1/'
''

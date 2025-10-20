{ jq, writeShellScriptBin }:
writeShellScriptBin "niri-window-select" ''
  if [ $# -eq 0 ]; then
    niri msg --json windows| ${jq}/bin/jq -r '.[]| (.id|tostring) + ": " + .app_id + " " + .title'
  else
    niri msg action focus-window --id ''${1%%:*} 1>/dev/null
  fi
''

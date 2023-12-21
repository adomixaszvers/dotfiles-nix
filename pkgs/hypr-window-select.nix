{ jq, writeShellScriptBin }:
writeShellScriptBin "hypr-window-select" ''
  if [ $# -eq 0 ]; then
    hyprctl clients -j| ${jq}/bin/jq -r '.[]| select(.pid != -1)| .address + ": " + "[" + .workspace.name + "] " + .initialClass + " " + .title'
  else
    hyprctl dispatch focuswindow address:''${1%%:*} 1>/dev/null
  fi
''

{
  jq,
  lib,
  writeShellScriptBin,
}:
writeShellScriptBin "hypr-window-select" ''
  if [ $# -eq 0 ]; then
    hyprctl clients -j| ${lib.getExe jq} -r '.[]| select(.pid != -1)| .address + ": " + "[" + .workspace.name + "] " + .initialClass + " " + .title'
  else
    hyprctl --batch -q "dispatch focuswindow address:''${1%%:*}; dispatch alterzorder top"
  fi
''

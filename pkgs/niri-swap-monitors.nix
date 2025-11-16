{
  writers,
  jq,
  lib,
}:
writers.writeDashBin "niri-swap-monitors" ''
  if [ "$(niri msg -j outputs | ${lib.getExe jq} length)" = 1 ]; then
    exit 0
  fi
  niri msg action do-screen-transition --delay-ms 150
  niri msg action move-workspace-to-monitor-next
  niri msg action focus-workspace-previous
  niri msg action move-workspace-to-monitor-previous
''

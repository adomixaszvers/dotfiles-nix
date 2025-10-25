{ writers }:
writers.writeDashBin "niri-swap-monitors" ''
  set -e
  niri msg action do-screen-transition --delay-ms 150
  niri msg action move-workspace-to-monitor-next
  niri msg action focus-workspace-previous
  niri msg action move-workspace-to-monitor-previous
''

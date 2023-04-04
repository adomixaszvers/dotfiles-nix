{ writeShellScriptBin }:
writeShellScriptBin "bspwm-greedy-focus" ''
  exit_with_error() {
    echo $1
    exit 1
  }

  [ $# -eq 1 ] || exit_with_error "usage: $0 [desktop to view]"

  desktop=$(bspc query -d "$1" -D) || exit_with_error "desktop $1 not found"

  # the desktop is on focused monitor. we just focus it
  if bspc query -d "$desktop" -m focused -D; then
    bspc desktop -f "$desktop"
  # the desktop is active on another monitor. we should swap
  elif bspc query -d "$desktop.active" -D; then
    focused_desktop=$(bspc query -d focused -D)
    bspc desktop "$desktop" -s "$focused_desktop"
    bspc desktop -a "$focused_desktop"
  # otherwise we move the desktop
  else
    bspc desktop "$desktop" -m focused
    bspc desktop -f "$desktop"
  fi 1>/dev/null

  exit 0
''

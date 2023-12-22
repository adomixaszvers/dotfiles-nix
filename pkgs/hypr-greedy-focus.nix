{ jq, writers }:
writers.writeDashBin "hypr-greedy-focus" ''
  PROG="''${0##*/}"

  set -e

  die() {
    echo "$PROG: $1" 1>&2
    exit 1
  }

  [ $# -eq 1 ] || die 'no desktop was supplied'

  WS=$1

  read -r WS_ACTIVE FOCUSED_OUTPUT FOCUSED_WS WS_OUTPUT <<EOF
  $(${jq}/bin/jq -rn --arg ws "$WS" --argjson workspaces "$(hyprctl -j workspaces)" --argjson monitors "$(hyprctl -j monitors)" '[
    ($monitors| map(select(.activeWorkspace.name == $ws))| any),
    ($monitors| .[]| select(.focused)| .id),
    ($monitors| .[]| select(.focused)| .activeWorkspace.name),
    ($workspaces| .[]| select(.name == $ws)| .monitorID)
  ]| @tsv')
  EOF

  echo "WS_ACTIVE=$WS_ACTIVE FOCUSED_OUTPUT=$FOCUSED_OUTPUT FOCUSED_WS=$FOCUSED_WS WS_OUTPUT=$WS_OUTPUT"

  if [ "$WS" = "$FOCUSED_WS" ]; then
    true
  elif [ -z "$WS_OUTPUT" ] || [ "$WS_OUTPUT" = "$FOCUSED_OUTPUT" ]; then
    # same output
    hyprctl dispatch workspace "$WS"
  elif [ "$WS_ACTIVE" = true ]; then
    hyprctl dispatch swapactiveworkspaces current "$WS_OUTPUT"
  else
    hyprctl --batch "dispatch moveworkspacetomonitor $WS current; dispatch workspace $WS"
  fi
''

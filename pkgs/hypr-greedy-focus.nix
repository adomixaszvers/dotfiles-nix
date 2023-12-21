{ jq, writers }:
writers.writeDashBin "hypr-greedy-focus" ''
  PROG="''${0##*/}"

  set -e

  die() {
    echo "$PROG: $1" 1>&2
    exit 1
  }

  [ $# -eq 1 ] || die 'no desktop was supplied'

  export WS=$1
  MONITORS="$(hyprctl -j monitors)"
  WORKSPACES="$(hyprctl -j workspaces)"
  export MONITORS WORKSPACES

  read -r WS_ACTIVE FOCUSED_OUTPUT WS_OUTPUT <<EOF
  $(${jq}/bin/jq -rn '[
    (env.MONITORS| fromjson| .[]| select(.activeWorkspace.name == env.WS)| any),
    (env.MONITORS| fromjson| .[]| select(.focused)| .id),
    (env.WORKSPACES| fromjson| .[]| select(.name == env.WS)| .monitorID)
  ]| @sh')
  EOF

  if [ -z "$WS_OUTPUT" ] || [ "$WS_OUTPUT" = "$FOCUSED_OUTPUT" ]; then
    # same output
    hyprctl dispatch workspace "$WS"
  elif [ "$WS_ACTIVE" = true ]; then
    hyprctl dispatch swapactiveworkspaces current "$WS_OUTPUT"
  else
    hyprctl --batch "dispatch moveworkspacetomonitor $WS current; dispatch workspace $WS"
  fi
''

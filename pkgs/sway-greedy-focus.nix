{ jq, writers }:
writers.writeDashBin "sway-greedy-focus" ''
  PROG="''${0##*/}"

  set -e

  die() {
    echo "$PROG: $1" 1>&2
    exit 1
  }

  [ $# -eq 1 ] || die 'no desktop was supplied'

  export WS=$1

  read -r WS_ACTIVE WS_OUTPUT FOCUSED_WS FOCUSED_OUTPUT <<EOF
    $(swaymsg -t get_tree| ${jq}/bin/jq --raw-output '(.nodes[].nodes[] | select(.name == env.WS) | .output ) as $ws_output|
      (.nodes[] | select(.name == $ws_output)| .current_workspace == env.WS) as $ws_active|
      (.nodes[].nodes[] | select(.. | .focused?)) as $focused | [
      $ws_active,
      $ws_output,
      ($focused| .name),
      ($focused| .output)
      ]| @tsv')
  EOF

  if [ -z "$WS_OUTPUT" ]; then
    # the workspace has to be created
    swaymsg workspace "$WS"
    exit 0
  fi

  # the workspace is already focused
  [ "$WS" = "$FOCUSED_WS" ] && exit 0

  if [ "$WS_OUTPUT" = "$FOCUSED_OUTPUT" ]; then
    # same output
    swaymsg "workspace $WS"
  elif [ "$WS_ACTIVE" = true ]; then
    # workspaces should be swapped on outputs
    swaymsg "[workspace=$FOCUSED_WS] move workspace to $WS_OUTPUT" || true # fails if empty ws
    swaymsg "[workspace=$WS] move workspace to $FOCUSED_OUTPUT"
  else
    # the workspace should be moved and that's it
    swaymsg "[workspace=$WS] move workspace to $FOCUSED_OUTPUT"
  fi
''

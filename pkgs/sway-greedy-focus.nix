{
  jq,
  writeShellScriptBin,
  coreutils,
  lib,
}:
writeShellScriptBin "sway-greedy-focus" ''
  PATH=${
    lib.makeBinPath [
      coreutils
      jq
    ]
  }:$PATH

  PROG=$(basename $0)

  set -ep

  die() {
    echo "$PROG: $1" 1>&2
    exit 1
  }

  [ $# -eq 1 ] || die 'no desktop was supplied'

  export WS=$1

  export SWAY_STATE=$(swaymsg -t get_tree)

  export WS_OUTPUT=$(jq -nr 'env.SWAY_STATE | fromjson | .nodes[].nodes[] | select(.name == env.WS) | .output')

  if [ -z "$WS_OUTPUT" ]; then
    # the workspace has to be created
    swaymsg workspace "$WS"
    exit 0
  fi

  WS_ACTIVE=$(jq -nr 'env.SWAY_STATE | fromjson | .nodes[] | select(.name == env.WS_OUTPUT)| .current_workspace == env.WS')
  FOCUSED_WS=$(jq -nr 'env.SWAY_STATE | fromjson | .nodes[].nodes[] | select(.. | .focused?) | .name')
  FOCUSED_OUTPUT=$(jq -nr 'env.SWAY_STATE | fromjson | .nodes[].nodes[] | select(.. | .focused?) | .output')

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

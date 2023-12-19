{ jq, writeShellScriptBin, coreutils, lib }:
writeShellScriptBin "hypr-greedy-focus" ''
  PATH=${lib.makeBinPath [ jq ]}:$PATH

  PROG=$(${coreutils}/bin/basename "$0")

  set -ep

  die() {
    echo "$PROG: $1" 1>&2
    exit 1
  }

  [ $# -eq 1 ] || die 'no desktop was supplied'

  export WS=$1

  WS_OUTPUT=$(hyprctl -j workspaces| jq -r '.[]|select(.name == env.WS)|.monitorID')
  export WS_OUTPUT

  if [ -z "$WS_OUTPUT" ]; then
    # the workspace has to be created
    hyprctl dispatch workspace "$WS"
    exit 0
  fi

  WS_ACTIVE=$(hyprctl -j monitors|jq -r '.[]|select(.activeWorkspace.name == env.WS)|any')
  FOCUSED_OUTPUT=$(hyprctl -j monitors| jq -r '.[]|select(.focused)|.id')

  if [ "$WS_OUTPUT" = "$FOCUSED_OUTPUT" ]; then
    # same output
    hyprctl dispatch workspace "$WS"
  elif [ "$WS_ACTIVE" = true ]; then
    hyprctl dispatch swapactiveworkspaces current "$WS_OUTPUT"
  else
    hyprctl --batch "dispatch moveworkspacetomonitor $WS current; dispatch workspace $WS"
  fi
''

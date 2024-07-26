{ writeShellScriptBin, lib, coreutils, maim, xdotool, xclip }:
let
  output = "$(date '+%y%m%d-%H%M-%S').png";
  xclip_cmd = "xclip -sel clip -t image/png";
in writeShellScriptBin "maimpick" ''
  PATH=$PATH:${lib.makeBinPath [ coreutils maim xdotool xclip ]}
  case "$(printf "a selected area\\ncurrent window\\nfull screen\\na selected area (copy)\\ncurrent window (copy)\\nfull screen (copy)" | rofi -dmenu -l 6 -i -p "Screenshot which area?")" in
    "a selected area") maim -u -s pic-selected-"${output}" ;;
    "current window") maim -B -q -d 0.2 -i "$(xdotool getactivewindow)" pic-window-"${output}" ;;
    "full screen") maim -q -d 0.2 pic-full-"${output}" ;;
    "a selected area (copy)") maim -u -s | ${xclip_cmd} ;;
    "current window (copy)") maim -q -d 0.2 -i "$(xdotool getactivewindow)" | ${xclip_cmd} ;;
    "full screen (copy)") maim -q -d 0.2 | ${xclip_cmd} ;;
  esac
''

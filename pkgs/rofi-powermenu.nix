{ writeShellScriptBin, rofi, lib }:
writeShellScriptBin "rofi-powermenu" ''
  PATH=$PATH:${lib.makeBinPath [ rofi ]}
  case "$(printf "lock session\\nlogout\\npoweroff\\nreboot"| rofi -dmenu -l 4 -i -p "Power menu")" in
    "lock session") loginctl lock-session ;;
    "logout") loginctl kill-session $XDG_SESSION_ID ;;
    "poweroff") systemctl poweroff ;;
    "reboot") systemctl reboot ;;
  esac
''

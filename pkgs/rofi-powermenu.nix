{ writeShellScriptBin, rofi, lib }:
writeShellScriptBin "rofi-powermenu" ''
  PATH=$PATH:${lib.makeBinPath [ rofi ]}

  rofi-question () {
    local question=$1
    [ $(printf "No\\nYes" | rofi -dmenu -l 2 -i -disable-history -p "$question") == Yes ]
  }

  case "$(printf "lock session\\nlogout\\npoweroff\\nreboot"| rofi -dmenu -l 4 -i -p "Power menu")" in
    "lock session") loginctl lock-session ;;
    "logout") rofi-question "Are you sure you want to logout?" && loginctl kill-session self ;;
    "poweroff") rofi-question "Are you sure you want to power off the computer?" && systemctl poweroff ;;
    "reboot") rofi-question "Are you sure you want to reboot the computer?" && systemctl reboot ;;
  esac
''

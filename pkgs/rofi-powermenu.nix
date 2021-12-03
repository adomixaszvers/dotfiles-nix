{ writeShellScriptBin, gnome, rofi, lib }:
writeShellScriptBin "rofi-powermenu" ''
  PATH=$PATH:${lib.makeBinPath [ rofi gnome.zenity ]}
  case "$(printf "lock session\\nlogout\\npoweroff\\nreboot"| rofi -dmenu -l 4 -i -p "Power menu")" in
    "lock session") loginctl lock-session ;;
    "logout") zenity --question --text="Are you sure you want to logout?" && loginctl kill-session $XDG_SESSION_ID ;;
    "poweroff") zenity --question --text="Are you sure you want to power off the computer?" && systemctl poweroff ;;
    "reboot") zenity --question --text="Are you sure you want to reboot the computer?" && systemctl reboot ;;
  esac
''

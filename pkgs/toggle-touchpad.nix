{
  writers,
  gnugrep,
  libinput,
  libnotify,
  lib,
}:
writers.writeDashBin "toggle-touchpad" ''
  # Toggle touchpad status
  # Using libinput and xinput
  PATH=${
    lib.makeBinPath [
      gnugrep
      libinput
      libnotify
    ]
  }:$PATH

  # Use xinput list and do a search for touchpads. Then get the first one and get its name.
  device="$(xinput list | grep -P '(?<= )[\w\s\/:]*(?i)(touchpad|synaptics)(?-i).*?(?=\s*id)' -o | head -n1)"

  # If it was activated disable it and if it wasn't disable it
  if [ "$(xinput list-props "$device" | grep -P ".*Device Enabled.*\K.(?=$)" -o)" = "1" ]; then
      xinput disable "$device" && notify-send -i touchpad "Touchpad disabled"
  else
      xinput enable "$device" && notify-send -i touchpad "Touchpad enabled"
  fi
''

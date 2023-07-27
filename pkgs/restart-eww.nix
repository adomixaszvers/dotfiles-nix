{ writers }:
writers.writeDashBin "restart-eww" ''
  eww kill
  eww daemon
  for i in $(xrandr --listactivemonitors| tail -n +2| cut -d':' -f 1); do
    eww open "bar$i"
  done
  echo eww restarted
''

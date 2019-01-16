{ pkgs, polybar, polybar-config}:
with pkgs;
writeScript "polybar-launch.sh"
''
# export PATH=$PATH:${lib.makeBinPath [i3-gaps polybar]}
PATH=$PATH:${lib.makeBinPath [gnugrep procps psmisc xorg.xrandr coreutils polybar]}

# More info : https://github.com/jaagr/polybar/wiki

# Install the following applications for polybar and icons in polybar if you are on ArcoLinuxD
# awesome-terminal-fonts
# Tip : There are other interesting fonts that provide icons like nerd-fonts-complete

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar > /dev/null; do sleep 1; done

desktop=$(echo i3)
count=$(xrandr --query | grep " connected" | cut -d" " -f1 | wc -l)
config=${polybar-config}


case $desktop in
    i3)
    if type "xrandr" > /dev/null; then
      for m in $(xrandr --query | grep " connected" | cut -d" " -f1); do
        MONITOR=$m polybar --reload mainbar-i3 -c $config &
      done
    else
    polybar --reload mainbar-i3 -c $config &
    fi
    ;;
    openbox)
    if type "xrandr" > /dev/null; then
      for m in $(xrandr --query | grep " connected" | cut -d" " -f1); do
        MONITOR=$m polybar --reload mainbar-openbox -c $config &
      done
    else
    polybar --reload mainbar-openbox -c $config &
    fi
#    if type "xrandr" > /dev/null; then
#      for m in $(xrandr --query | grep " connected" | cut -d" " -f1); do
#        MONITOR=$m polybar --reload mainbar-openbox-extra -c $config &
#      done
#    else
#    polybar --reload mainbar-openbox-extra -c $config &
#    fi

    ;;
    bspwm)
    if type "xrandr" > /dev/null; then
      for m in $(xrandr --query | grep " connected" | cut -d" " -f1); do
        MONITOR=$m polybar --reload mainbar-bspwm -c $config &
      done
    else
    polybar --reload mainbar-bspwm -c $config &
    fi
    ;;

    xmonad)
    if [ $count = 1 ]; then
      m=$(xrandr --query | grep " connected" | cut -d" " -f1)
      MONITOR=$m polybar --reload mainbar-xmonad -c $config &
    else
      for m in $(xrandr --query | grep " connected" | cut -d" " -f1); do
        MONITOR=$m polybar --reload mainbar-xmonad -c $config &
      done
    fi
    ;;
esac

#for future scripts - how to find interface
#interface-name=$(ip route | grep '^default' | awk '{print $5}')
#interface-name=$(ifconfig -a | sed -n 's/^\([^ ]\+\).*/\1/p' | grep -Fvx -e lo:| sed 's/.$//')
''

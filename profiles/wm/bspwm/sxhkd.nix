{ config, myPkgs, ... }:
{

  services.sxhkd = {
    enable = true;
    package = myPkgs.sxhkd;
    keybindings =
      let
        terminal = config.home.sessionVariables.TERMINAL;
      in
      {
        #
        # wm independent hotkeys
        #
        "Print" = "maimpick";

        # scratchpads
        "super + ctrl + s" = ''
          tdrop --wm=bspwm --name=scratchpad -f "--name scratchpad" ${terminal}
        '';
        "super + ctrl + e" = ''
          tdrop --wm=bspwm --name=scratchpad -f "--name scratchpad --fullscreen" emacs
        '';

        # terminal emulator
        "super + Return" = terminal;

        # program launcher
        "super + d" = "rofi -show combi -combi-modi window,drun -show-icons";
        "super + shift + d" = "rofi -show run -sidebar-mode";

        # music hotkeys
        "{XF86AudioPlay,XF86AudioPrev,XF86AudioNext}" = "playerctl {play-pause,previous,next}";
        "super + {F5,F6,F7}" = "playerctl {play-pause,previous,next}";

        # dunst
        "super + {_,shift + } + F9" = "dunstctl close{_,-all}";
        "super + F10" = "dunstctl history-pop";
        "super + F11" = "dunstctl context";

        # power menu
        "super + F4" = "rofi-powermenu";

        # make sxhkd reload its configuration files:
        "super + Escape" = "pkill -USR1 -x sxhkd";

        #
        # bspwm hotkeys
        #

        # quit bspwm normally
        "super + shift + c" = "bspc quit";

        # close and kill
        "super + {shift,ctrl} + q" = "bspc node -{c,k}";

        # alternate between the tiled and monocle layout
        "super + m" = "bspc desktop -l next";

        # send the newest marked node to the newest preselected node
        "super + y" = "bspc node newest.marked.local -n newest.!automatic.local";

        # swap the current node and the biggest node
        "super + g" = "bspc node -s biggest";

        #
        # state/flags
        #

        # set the window state
        "super + {t,shift + t,s,f}" = "bspc node -t {tiled,pseudo_tiled,floating,fullscreen}";

        # set the node flags
        "super + ctrl + {m,x,y,z}" = "bspc node -g {marked,locked,sticky,private}";

        #
        # focus/swap
        #

        # focus the node in the given direction
        "super + {_,shift + }{h,j,k,l}" = "bspc node -{f,s} {west,south,north,east}";

        # focus the node for the given path jump
        "super + {p,b,comma,period}" = "bspc node -f @{parent,brother,first,second}";

        # focus the next/previous node in the current desktop
        "super + {_,shift + }c" = "bspc node -f {next,prev}.local.leaf";

        # focus the next/previous desktop in the current monitor
        "super + bracket{left,right}" = "bspc desktop -f {prev,next}.local";

        # move desktop to next/previous monitor
        "super + ctrl + bracket{left,right}" = ''
          ; bspc monitor {prev,next} -f
        '';

        # focus the last node/desktop
        "super + {grave,Tab}" = "bspc {node,desktop} -f last";

        # focus the older or newer node in the focus history
        "super + {o,i}" = "bspc wm -h off; bspc node {older,newer} -f; bspc wm -h on";

        # focus or send to the given desktop
        "super + {1-9,0}" = "bspwm-greedy-focus '{1-9,10}' && bspwm-reorder-desktops";
        "super + {aogonek,ccaron,eogonek,eabovedot,iogonek,scaron,uogonek,umacron,doublelowquotemark,leftdoublequotemark}" =
          "bspwm-greedy-focus '{1,2,3,4,5,6,7,8,9,10}' && bspwm-reorder-desktops";

        "super + shift {1-9,0}" = "bspc node -d '{1-9,10}'";
        "super + shift {aogonek,ccaron,eogonek,eabovedot,iogonek,scaron,uogonek,umacron,doublelowquotemark,leftdoublequotemark}" =
          "bspc node -d '{1,2,3,4,5,6,7,8,9,10}'";

        #
        # preselect
        #

        # preselect the direction
        "super + ctrl + {h,j,k,l}" = "bspc node -p {west,south,north,east}";

        # preselect the ratio
        "super + ctrl + {aogonek,ccaron,eogonek,eabovedot,iogonek,scaron,uogonek,umacron,doublelowquotemark}" =
          "bspc node -o 0.{1-9}";
        "super + ctrl + {1-9}" = "bspc node -o 0.{1-9}";

        # cancel the preselection for the focused node
        "super + ctrl + space" = "bspc node -p cancel";

        # cancel the preselection for the focused desktop
        "super + ctrl + shift + space" = "bspc query -N -d | xargs -I id -n 1 bspc node id -p cancel";

        #
        # move/resize
        #

        # expand a window by moving one of its side outward
        "super + alt + {h,j,k,l}" = "bspc node -z {left -20 0,bottom 0 20,top 0 -20,right 20 0}";

        # contract a window by moving one of its side inward
        "super + alt + shift + {h,j,k,l}" = "bspc node -z {right -20 0,top 0 20,bottom 0 -20,left 20 0}";

        # move a floating window
        "super + {Left,Down,Up,Right}" = "bspc node -v {-20 0,0 20,0 -20,20 0}";

        # volume controls
        "{XF86AudioMute,XF86AudioLowerVolume,XF86AudioRaiseVolume}" = "pamixer {-t,-d 5,-i 5}";
        "super + {minus,equal,zcaron}" = "pamixer {-d 5,-i 5,-i 5}";

        # brightness controls
        "{XF86MonBrightnessDown,XF86MonBrightnessUp}" = "brightnessctl set {5%-,5%+}";
      };
  };
}

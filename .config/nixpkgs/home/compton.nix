{ pkgs, ... }:
{
  services.compton = {
    enable = true;
    extraOptions = ''
      backend = "xrender";
      xrender-sync = true;
      xrender-sync-fence = true;

      shadow = true;
      no-dnd-shadow = true;
      no-dock-shadow = false;
      clear-shadow = true;
      shadow-radius = 2;
      shadow-offset-x = -3;
      shadow-offset-y = -2;
      shadow-opacity = 0.30;
      shadow-red = 0.03;
      shadow-green = 0.03;
      shadow-blue = 0.03;
      shadow-exclude = [
      "name *?= 'shadowless'",
      "name *?= 'xdrawrect-ws-separator-'",
      "class_g *?= 'presel_feedback'",
      "class_g *?= 'lemonbar'",
      "class_g *?= 'Bspwm'",
      "name *?= 'polybar-top_eDP-1'",
      "name *?= 'polybar-bottom_eDP-1'",
      "name *?= 'polybar-external_bottom_HDMI-1'"
      ];
      shadow-ignore-shaped = false;
      menu-opacity = 1;
      inactive-opacity = 0.99;
      frame-opacity = 0.6;
      alpha-step = 0.05;
      inactive-dim = 0.06;
      opacity-rule = [
      "20:_NET_WM_STATE@:32a *= '_NET_WM_STATE_HIDDEN'",
      "100:_NET_WM_STATE@:32a *= '_NET_WM_STATE_FULLSCREEN'",
      "99:class_g *?= 'chromium*app'",
      "99:class_g *?= 'Chromium'",
      "99:class_g *?= 'Tor Browser'",
      "99:class_g *?= 'irssi'",
      "20:class_g *?= 'Bspwm'",
      "100:class_g *?= 'Gimp'"
      ];
      blur-kern = "7x7box"
      resize-damage = 2
      blur-background-frame = false;
      blur-background-fixed = false;
      blur-background-exclude = [];
      fading = true;
      fade-delta = 8;
      fade-in-step = 0.07;
      fade-out-step = 0.07;
      no-fading-openclose = false;
      fade-exclude = [
      "_NET_WM_STATE@:32a *= '_NET_WM_STATE_HIDDEN'"
      ];
      detect-rounded-corners = true;
      detect-client-opacity = true;
      unredir-if-possible = true;
      focus-exclude = [
        "name *?= 'jetbrains'",
        "name *?= 'sun-awt-X11'",
        "class_g *?= 'wine'",
        "name *?= 'Unigine'",
        "name *?= 'Steam'",
        "class_g *?= 'Steam'",
        "name *?= 'Android'",
        "name *?= 'Dunst'",
        "name *?= 'tmp/flat'",
        "class_g *?= 'xdrawrect'",
        "name *?= 'Authy'"
      ];
      detect-transient = true;
      detect-client-leader = true;
      wintypes: {
        tooltip = {
          fading = true;
          shadow = false;
          opacity = 0.95;
          focus = true;
        };
        menu = {
          fade = true;
          shadow = true;
          opacity = 1;
          focus = true;
        };
      };
      '';
    };
  }

{ pkgs, config }:
let
  gaps = {
    inner = 15;
    outer = 0;
  };
in {
  config = let
    modifier = "Mod4";
    workspace1 = "1";
    workspace2 = "2";
    workspace3 = "3";
    workspace4 = "4";
    workspace5 = "5";
    workspace6 = "6";
    workspace7 = "7";
    workspace8 = "8";
    workspace9 = "9";
    workspace10 = "10";
  in {

    bars = [{
      statusCommand = ''
        ${pkgs.i3status-rust}/bin/i3status-rs ${./status_config.toml}
      '';
      fonts = [ "NotoMono Nerd Font 9" ];
      colors = with config.colors; {
        inherit background;
        activeWorkspace = {
          inherit background;
          border = background;
          text = whiteb;
        };
        focusedWorkspace = {
          background = blackb;
          border = background;
          text = whiteb;
        };
        inactiveWorkspace = {
          inherit background;
          border = background;
          text = white;
        };
        statusline = white;
        urgentWorkspace = {
          background = redb;
          border = red;
          text = whiteb;
        };
      };
      position = "top";
    }];
    # bars = [];
    colors = with config.colors; {
      background = black;
      focused = {
        inherit background;
        border = background;
        text = whiteb;
        indicator = blackb;
        childBorder = cyan;
      };
      unfocused = {
        background = black;
        border = black;
        text = white;
        indicator = blackb;
        childBorder = blackb;
      };
      focusedInactive = {
        background = black;
        border = black;
        text = white;
        indicator = blackb;
        childBorder = blackb;
      };
      urgent = {
        background = redb;
        border = redb;
        text = black;
        indicator = redb;
        childBorder = redb;
      };
    };
    fonts = [ "NotoMono Nerd Font 8" ];
    gaps = {
      inherit (gaps) inner outer;
      smartGaps = true;
      smartBorders = "on";
    };
    inherit modifier;
    assigns = {
      "${workspace1}" =
        [ { class = "^Google-chrome$"; } { class = "^Firefox$"; } ];
      "${workspace3}" = [{ class = "^(Atom|jetbrains-idea)$"; }];
      "${workspace4}" = [ { class = "^Skype$"; } { class = "^rambox$"; } ];
      "${workspace5}" = [ { class = "^Steam$"; } { class = "^SmartGit"; } ];
      "${workspace6}" = [{ class = "^libreoffice"; }];
      "${workspace7}" = [{ class = "^Emacs"; }];
      "${workspace10}" = [{ class = "^Spotify"; }];
    };
    keybindings = {
      "${modifier}+Return" = "exec termite";
      "${modifier}+Shift+q" = "kill";

      "${modifier}+h" = "focus left";
      "${modifier}+j" = "focus down";
      "${modifier}+k" = "focus up";
      "${modifier}+l" = "focus right";

      "${modifier}+Shift+h" = "move left";
      "${modifier}+Shift+j" = "move down";
      "${modifier}+Shift+k" = "move up";
      "${modifier}+Shift+l" = "move right";

      "${modifier}+Left" = "focus left";
      "${modifier}+Down" = "focus down";
      "${modifier}+Up" = "focus up";
      "${modifier}+Right" = "focus right";

      "${modifier}+Shift+Left" = "move left";
      "${modifier}+Shift+Down" = "move down";
      "${modifier}+Shift+Up" = "move up";
      "${modifier}+Shift+Right" = "move right";

      "${modifier}+b" = "split h";
      "${modifier}+v" = "split v";
      "${modifier}+f" = "fullscreen toggle";

      "${modifier}+s" = "layout stacking";
      "${modifier}+w" = "layout tabbed";
      "${modifier}+e" = "layout toggle split";

      "${modifier}+Shift+space" = "floating toggle";
      "${modifier}+space" = "focus mode_toggle";
      "${modifier}+a" = "focus parent";
      "Shift+${modifier}+a" = "focus child";

      "${modifier}+Shift+c" = "reload";
      "${modifier}+Shift+r" = "restart";

      "${modifier}+r" = "mode resize";
      "${modifier}+m" = "move workspace to output left";

      "${modifier}+g" = "gaps inner current set ${
          toString gaps.inner
        }; gaps outer current set ${toString gaps.outer}";
      "${modifier}+Shift+g" =
        "gaps inner current set 0; gaps outer current set 0";

      "Print" = "exec --no-startup-id maimpick";
    };

    keycodebindings = {
      "${modifier}+10" = "workspace ${workspace1}";
      "${modifier}+11" = "workspace ${workspace2}";
      "${modifier}+12" = "workspace ${workspace3}";
      "${modifier}+13" = "workspace ${workspace4}";
      "${modifier}+14" = "workspace ${workspace5}";
      "${modifier}+15" = "workspace ${workspace6}";
      "${modifier}+16" = "workspace ${workspace7}";
      "${modifier}+17" = "workspace ${workspace8}";
      "${modifier}+18" = "workspace ${workspace9}";
      "${modifier}+19" = "workspace ${workspace10}";

      "Shift+${modifier}+10" = "move container to workspace ${workspace1}";
      "Shift+${modifier}+11" = "move container to workspace ${workspace2}";
      "Shift+${modifier}+12" = "move container to workspace ${workspace3}";
      "Shift+${modifier}+13" = "move container to workspace ${workspace4}";
      "Shift+${modifier}+14" = "move container to workspace ${workspace5}";
      "Shift+${modifier}+15" = "move container to workspace ${workspace6}";
      "Shift+${modifier}+16" = "move container to workspace ${workspace7}";
      "Shift+${modifier}+17" = "move container to workspace ${workspace8}";
      "Shift+${modifier}+18" = "move container to workspace ${workspace9}";
      "Shift+${modifier}+19" = "move container to workspace ${workspace10}";
    };
    modes = {
      resize = {
        Escape = "mode default";
        Return = "mode default";
        h = "resize shrink width 10 px or 10 ppt";
        j = "resize grow height 10 px or 10 ppt";
        k = "resize shrink height 10 px or 10 ppt";
        l = "resize grow width 10 px or 10 ppt";
      };
    };
    window.border = 3;
    window.commands = [
      {
        command = "move to workspace ${workspace10}";
        criteria = { class = "Spotify"; };
      }
      {
        command = "move to workspace ${workspace6}";
        criteria = { class = "libreoffice"; };
      }
    ];
  };
}

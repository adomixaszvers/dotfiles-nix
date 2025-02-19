{
  programs.waybar = {
    enable = true;
    # style = builtins.readFile ./style.css;
    style = # css
      ''
        * {
          padding-top: 0;
          padding-bottom: 0;
          min-height: 0;
        }
      '';
    settings.mainbar = {
      battery = {
        format = "{icon} {capacity}%";
        format-icons = [
          "󰂎"
          "󰁺"
          "󰁻"
          "󰁼"
          "󰁽"
          "󰁾"
          "󰁿"
          "󰂀"
          "󰂁"
          "󰂂"
          "󰁹"
        ];
        # format-icons = [
        #   ""
        #   ""
        #   ""
        #   ""
        #   ""
        # ];
      };
      clock = {
        format = "{:%Y-%m-%d %H:%M}";
        tooltip-format = "<tt><small>{calendar}</small></tt>";
        calendar = {
          mode = "year";
          mode-mon-col = 3;
          weeks-pos = "right";
          on-scroll = 1;
          format = {
            months = "<span color='#ffead3'><b>{}</b></span>";
            days = "<span color='#ecc6d9'><b>{}</b></span>";
            weeks = "<span color='#99ffdd'><b>W{}</b></span>";
            weekdays = "<span color='#ffcc66'><b>{}</b></span>";
            today = "<span color='#ff6699'><b><u>{}</u></b></span>";
          };
        };
        actions = {
          on-click-right = "mode";
          on-scroll-up = [
            "tz_up"
            "shift_up"
          ];
          on-scroll-down = [
            "tz_down"
            "shift_down"
          ];
        };
      };
      pulseaudio = {
        format = "{icon} {volume}%";
        format-icons = {
          default = [
            "󰕿"
            "󰖀"
            "󰕾"
          ];
          default-muted = "󰝟";
        };
        scroll-step = 5.0;
      };
      cpu.format = "󰘚 {usage}%";
      memory.format = "󰍛 {percentage}%";
    };
  };
}

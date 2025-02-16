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
      };
      cpu.format = "󰘚 {usage}%";
      memory.format = "󰍛 {percentage}%";
    };
  };
}

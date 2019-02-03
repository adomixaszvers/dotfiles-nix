{ pkgs, config, ... }:
let colors = config.lib.colors;
in
{
  services.dunst = {
    enable = true;
    settings = {
      global = {
        alignment = "center";
        allow_markup = true;
        bounce_freq = 0;
        dmenu = "${pkgs.rofi}/bin/rofi -dmenu -p dunst";
        follow = "keyboard";
        font = "NotoMono Nerd Font 10";
        format = "<b>%s</b>\n%b";
        geometry = "300x5-30+20";
        horizontal_padding = 8;
        idle_threshold = 120;
        ignore_newline = false;
        indicate_hidden = true;
        line_height = 0;
        markup = "full";
        monitor = 0;
        padding = 8;
        separator_color = "#585858";
        separator_height = 2;
        show_age_threshold = 60;
        sort = true;
        startup_notification = true;
        sticky_history = true;
        transparency = 0;
        word_wrap = true;
        icon_position = "left";
      };
      frame = {
        width = 1;
        color = colors.foreground;
      };
      shortcuts = {
        close = "ctrl+space";
        close_all = "ctrl+shift+space";
        history = "ctrl+grave";
        context = "ctrl+shift+period";
      };
      urgency_low = {
        background = colors.black;
        foreground = colors.white;
        timeout = 10;
      };
      urgency_normal = {
        background = colors.background;
        foreground = colors.whiteb;
        timeout = 900;
      };
      urgency_critical = {
        background = colors.red;
        foreground = colors.whiteb;
        timeout = 0;
      };
    };
  };
}

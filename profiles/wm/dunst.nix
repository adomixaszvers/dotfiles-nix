{ pkgs, config, ... }:
{
  home.packages = with pkgs; [ dunst ];
  services.dunst = {
    enable = true;
    iconTheme = {
      name = "Papirus-Dark";
      package = pkgs.papirus-icon-theme;
      size = "32x32";
    };
    settings = {
      global = {
        alignment = "center";
        allow_markup = true;
        bounce_freq = 0;
        browser = "${pkgs.xdg-utils}/bin/xdg-open";
        dmenu = "${config.programs.rofi.finalPackage}/bin/rofi -dmenu -p dunst";
        follow = "keyboard";
        format = "<b>%s</b>\\n%b";
        geometry = "300x5-30+20";
        horizontal_padding = 8;
        idle_threshold = 120;
        ignore_newline = false;
        indicate_hidden = true;
        line_height = 0;
        markup = "full";
        monitor = 0;
        padding = 8;
        separator_height = 2;
        show_age_threshold = 60;
        sort = true;
        startup_notification = false;
        sticky_history = true;
        transparency = 0;
        word_wrap = true;
        icon_position = "left";
        max_icon_size = "32";
      };
      frame = {
        width = 1;
      };
      urgency_low = {
        timeout = 10;
      };
      urgency_normal = {
        timeout = 60;
      };
      urgency_critical = {
        timeout = 0;
      };
    };
  };
  stylix.targets.dunst.enable = true;
}

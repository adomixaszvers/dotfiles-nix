{
  programs.i3status-rust = {
    enable = true;
    bars = {
      default = {
        theme = "nord-dark";
        icons = "material-nf";
        blocks = [
          {
            block = "focused_window";
            max_width = 50;
            show_marks = "visible";
          }
          {
            block = "disk_space";
            path = "/";
            alias = "/";
            info_type = "available";
            unit = "GB";
            interval = 20;
            warning = 20.0;
            alert = 10.0;
          }
          {
            block = "memory";
            display_type = "memory";
            format_mem = "{mem_used_percents}%";
          }
          {
            block = "load";
            interval = 1;
            format = "{1m}";
          }
          { block = "sound"; }
          {
            block = "time";
            interval = 60;
            format = "%Y-%m-%d %R";
          }
        ];
      };
    };
  };
}

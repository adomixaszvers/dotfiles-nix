{ config, ... }: {
  programs.kitty = {
    enable = true;
    settings = with config.colors; {
      allow_remote_control = true;

      enable_audio_bell = false;

      font_family = "JetBrainsMono Nerd Font Mono";
      # font_size = "9.0";

      inherit foreground background;
      background_opacity = "1.0";

      # black
      color0 = black;
      color8 = blackb;

      # red
      color1 = red;
      color9 = redb;

      # green
      color2 = green;
      color10 = greenb;

      # yellow
      color3 = yellow;
      color11 = yellowb;

      # blue
      color4 = blue;
      color12 = blueb;

      # magenta
      color5 = magenta;
      color13 = magentab;

      # cyan
      color6 = cyan;
      color14 = cyanb;

      # white
      color7 = white;
      color15 = whiteb;

      envinclude = "KITTY_CONF_*";

    };
  };
  programs.zsh.initExtra = ''
    if [ "$TERM" = 'xterm-kitty' ] && [ -x "$(command -v kitty)" ] && [ -n "$DISPLAY" ]; then
      alias ssh='kitty +kitten ssh'
    fi
  '';
}

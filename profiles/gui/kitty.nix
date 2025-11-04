{
  programs.kitty = {
    enable = true;
    settings = {
      allow_remote_control = true;
      bold_font = "auto";
      italic_font = "auto";
      bold_italic_font = "auto";
      enable_audio_bell = false;
      envinclude = "KITTY_CONF_*";

    };
  };
  # does not work with openssh 10.1
  # programs.zsh.initContent = # bash
  #   ''
  #     if [ "$TERM" = 'xterm-kitty' ] && [ -x "$(command -v kitty)" ] && [ -n "$DISPLAY" ]; then
  #       alias ssh='kitty +kitten ssh'
  #     fi
  #   '';
  stylix.targets.kitty = {
    enable = true;
    # workaround for bright green looking like black color
    variant256Colors = true;
  };
}

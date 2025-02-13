{
  programs.waybar = {
    enable = true;
    # style = builtins.readFile ./style.css;
    style = ''
      * {
        min-height: 0;
      }
    '';
  };
}

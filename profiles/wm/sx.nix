{ pkgs, config, ... }: {
  xdg.configFile."sx/sxrc" = {
    executable = true;
    text = config.home.file.".xsession".text;
  };
  home.packages = [ pkgs.sx ];
}

{ config, ... }: {
  xdg.configFile."sx/sxrc" = {
    executable = true;
    inherit (config.home.file.".xsession") text;
  };
}

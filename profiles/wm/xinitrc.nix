{ config, ... }: {
  home.file.".xinitrc" = {
    executable = true;
    inherit (config.home.file.".xsession") text;
  };
}

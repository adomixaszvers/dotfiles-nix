{ pkgs, myPkgs, ... }: {
  imports = [ ../waybar ];
  home.packages = let
    launch-river = pkgs.writers.writeDashBin "launch-river" ''
      export MOZ_ENABLE_WAYLAND=1
      export _JAVA_AWT_WM_NONREPARENTING=1
      export XKB_DEFAULT_LAYOUT=lt,us
      exec river
    '';
  in [ launch-river pkgs.kanshi pkgs.river pkgs.wofi myPkgs.rivercarro ];
  programs.waybar = {
    settings = [{
      layer = "top";
      position = "top";
      height = 16;
      modules-left = [ "river/tags" ];
      modules-right = [ "pulseaudio" "cpu" "memory" "temperature" "clock" ];
      modules = {
        "river/tags" = { "num-tags" = 9; };
        "clock" = { format = "{:%Y-%m-%d %H:%M}"; };
        "pulseaudio" = { scroll-step = "5.0"; };
        temperature.thermal-zone = 1;
      };
    }];
  };
  services.kanshi.enable = true;
  xdg.configFile."river/init" = {
    source = ./river-init;
    executable = true;
  };
}

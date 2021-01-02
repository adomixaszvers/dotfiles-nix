{ pkgs, config, lib, ... }:
let common = import ./common.nix { inherit pkgs config; };
in {
  imports = [ ../picom.nix ../dunst.nix ];
  xsession.windowManager.i3 = {
    enable = true;
    config = let modifier = common.config.modifier;
    in common.config // {
      inherit (common.config)
        bars colors fonts gaps modifier assigns window keycodebindings;
      keybindings = let
        combined = common.config.keybindings // {
          "Shift+${modifier}+d" =
            "exec ${pkgs.rofi}/bin/rofi -show run -sidebar-mode";
          "${modifier}+d" = ''
            exec ${pkgs.rofi}/bin/rofi -show combi -combi-modi "window#drun" -modi combi'';
          "${modifier}+Tab" = "exec ${pkgs.rofi}/bin/rofi -show window";

          "${modifier}+r" = "mode resize";
          "${modifier}+F4" =
            "exec --no-startup-id ${pkgs.mine.rofi-powermenu}/bin/rofi-powermenu";
          "${modifier}+m" = "move workspace to output left";

          "Print" = "exec --no-startup-id maimpick";
        };
      in lib.mkOptionDefault combined;
      startup = [{
        command = "feh --bg-max --image-bg white --no-fehbg ~/wallpaper.png";
        always = true;
        notification = false;
      }
      # { command = with (import ./polybar {inherit pkgs config; }); "${launch}"; always = true; notification = false; }
      # { command = "polybar-msg cmd restart"; always = true; notification = false; }
        ];
    };
    extraConfig = ''
      for_window [title="Darbalaukis — Plasma"] kill; floating enable; border none
      for_window [class="plasmashell"] floating enable;
      for_window [class="Plasma"] floating enable; border none
      for_window [title="plasma-desktop"] floating enable; border none
      for_window [title="win7"] floating enable; border none
      for_window [class="krunner"] floating enable; border none
      for_window [class="Kmix"] floating enable; border none
      for_window [class="Klipper"] floating enable; border none
      for_window [class="Plasmoidviewer"] floating enable; border none
      for_window [class="(?i)*nextcloud*"] floating disable
      for_window [class="plasmashell" window_type="notification"] floating enable, border none, move right 700px, move down 450px, no_focus
    '';
  };
}

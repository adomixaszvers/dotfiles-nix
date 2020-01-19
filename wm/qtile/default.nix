{ pkgs, ... }:

{
  imports = [ ../compton.nix ../dunst.nix ];
  home.packages = (with pkgs.channels.nixos-unstable; [
    qtile
  ]) ++ (with pkgs; [ mine.rofi-powermenu ]);
  xsession.windowManager.command = "qtile";
  xdg.configFile."qtile/config.py" = {
    source = ./config.py;
    onChange = "qtile-cmd -o cmd -f restart 2>/dev/null || true";
  };
}

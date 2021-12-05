{ pkgs, ... }:

{
  home.packages = let
    padding = pkgs.writeShellScriptBin "padding-icon.sh"
      (builtins.readFile ./padding-icon.sh);
  in [ pkgs.xmobar padding ];
  xdg.configFile."xmobar/xmobarrc".source = ./xmobar.config;
}

{
  pkgs,
  config,
  inputs,
  lib,
  ...
}:
{
  home.packages =
    let
      ghosttyPkgs = builtins.getAttr pkgs.hostPlatform.system inputs.ghostty.packages;
    in
    [ ghosttyPkgs.ghostty ];
  xdg.configFile."ghostty/config".text =
    let
      inherit (config.stylix) fonts opacity;
      inherit (config.lib.stylix) scheme;
    in
    ''
      font-family = ${fonts.monospace.name}
      font-family = ${fonts.emoji.name}
      font-size = ${toString fonts.sizes.terminal}
      background-opacity = ${toString opacity.terminal}
      gtk-titlebar = false
      window-decoration = false
      theme = ${scheme.slug}
    '';
}

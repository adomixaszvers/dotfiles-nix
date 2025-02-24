{
  config,
  lib,
  ...
}:
{
  programs.ghostty = {
    enable = lib.mkDefault false;
    settings =
      let
        inherit (config.stylix) fonts opacity;
        inherit (config.lib.stylix) scheme;
      in
      {
        font-family = [
          fonts.monospace.name
          fonts.emoji.name
        ];
        font-size = fonts.sizes.terminal;
        background-opacity = opacity.terminal;
        theme = scheme.slug;
        keybind = [
          "ctrl+shift+s=paste_from_selection"
        ];
      };
  };
}

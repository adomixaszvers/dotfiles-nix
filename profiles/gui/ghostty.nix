{
  config,
  lib,
  ...
}:
{
  programs.ghostty = {
    enable = lib.mkDefault true;
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
      };
  };
}

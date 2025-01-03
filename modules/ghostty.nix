{
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.programs.ghostty;
  keyValueSettings = {
    listsAsDuplicateKeys = true;
    mkKeyValue = lib.generators.mkKeyValueDefault { } " = ";
  };
  keyValue = pkgs.formats.keyValue keyValueSettings;
in
{
  options.programs.ghostty = {
    enable = lib.mkEnableOption "Enable ghostty";
    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.ghostty;
      defaultText = lib.literalExpression "pkgs.ghostty";
      description = ''
        Ghostty package to install.
      '';
    };
    settings = lib.mkOption {
      inherit (keyValue) type;
      default =
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
      example = lib.literalExpression ''
        {
          theme = "catppuccin-mocha";
          font-size = 10;
        }
      '';
      description = ''
        Configuration written to {file}`$XDG_CONFIG_HOME/ghostty/config`.
        See <https://ghostty.org/docs/config/reference> for more information.
      '';
    };
  };
  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.package ];
    xdg.configFile."ghostty/config" = lib.mkIf (cfg.settings != { }) {
      source = keyValue.generate "ghostty-config" cfg.settings;
    };
  };
}

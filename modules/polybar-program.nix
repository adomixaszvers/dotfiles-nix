{ config, options, lib, pkgs, ... }:
with lib;

let

  cfg = config.services.polybar;
  opt = options.services.polybar;

  # Convert a key/val pair to the insane format that polybar uses.
  # Each input key/val pair may return several output key/val pairs.
  convertPolybarKeyVal = key: val:
    # Convert { foo = [ "a" "b" ]; }
    # to {
    #   foo-0 = "a";
    #   foo-1 = "b";
    # }
    if isList val then
      concatLists (imap0 (i: convertPolybarKeyVal "${key}-${toString i}") val)
      # Convert {
      #   foo.text = "a";
      #   foo.font = 1;
      # } to {
      #   foo = "a";
      #   foo-font = 1;
      # }
    else if isAttrs val && !lib.isDerivation val then
      concatLists (mapAttrsToList
        (k: convertPolybarKeyVal (if k == "text" then key else "${key}-${k}"))
        val)
      # Base case
    else
      [ (nameValuePair key val) ];

  convertPolybarSection = _: attrs:
    listToAttrs (concatLists (mapAttrsToList convertPolybarKeyVal attrs));

  # Converts an attrset to INI text, quoting values as expected by polybar.
  # This does no more fancy conversion.
  toPolybarIni = generators.toINI {
    mkKeyValue = key: value:
      let
        quoted = v:
          if hasPrefix " " v || hasSuffix " " v then ''"${v}"'' else v;

        value' = if isBool value then
          (if value then "true" else "false")
        else if (isString value && key != "include-file") then
          quoted value
        else
          toString value;
      in "${key}=${value'}";
  };

  configFile = let
    isDeclarativeConfig = cfg.settings != opt.settings.default || cfg.config
      != opt.config.default || cfg.extraConfig != opt.extraConfig.default;
  in if isDeclarativeConfig then
    pkgs.writeText "polybar.conf" ''
      ${toPolybarIni cfg.config}
      ${toPolybarIni (mapAttrs convertPolybarSection cfg.settings)}
      ${cfg.extraConfig}
    ''
  else
    null;
in {
  options.programs.polybar.enable = mkEnableOption "Polybar status bar";
  config = mkIf config.programs.polybar.enable {
    home.packages = [ cfg.package ];
    xdg.configFile."polybar/config.ini" = mkIf (configFile != null) {
      source = configFile;
      onChange = # bash
        ''
          ${pkgs.procps}/bin/pkill -USR1 -u $USER .polybar-wrappe || true
        '';

    };
  };

}

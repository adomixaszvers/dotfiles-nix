{
  inputs,
  lib,
  config,
  ...
}:
{
  xdg.configFile = lib.attrsets.mapAttrs' (n: v: {
    name = "flakeInputs/${n}";
    value = {
      source = v.outPath;
    };
  }) (builtins.removeAttrs inputs [ "self" ]);
  home.sessionVariables."NIX_PATH" = "${config.home.sessionVariables.XDG_CONFIG_HOME}/flakeInputs\${NIX_PATH:+:}$NIX_PATH";
}

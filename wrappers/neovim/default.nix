{
  config,
  wlib,
  lib,
  ...
}:
{
  imports = [ wlib.wrapperModules.neovim ];

  options.settings.cats = lib.mkOption {
    readOnly = true;
    type = lib.types.attrsOf lib.types.raw;
    default = builtins.mapAttrs (_: v: v.enable) config.specs;
  };

  config = {
    hosts = {
      python3.nvim-host.enable = false;
      node.nvim-host.enable = false;
      ruby.nvim-host.enable = false;
    };
    settings = {
      config_directory = builtins.path {
        name = "my-neovim-config";
        path = ./luapath;
      };
    };
    # This submodule modifies both levels of your specs
    specMods = _: {
      options.extraPackages = lib.mkOption {
        type = lib.types.listOf wlib.types.stringable;
        default = [ ];
        description = "a extraPackages spec field to put packages to suffix to the PATH";
      };
    };
    extraPackages = config.specCollect (acc: v: acc ++ (v.extraPackages or [ ])) [ ];
  };
}

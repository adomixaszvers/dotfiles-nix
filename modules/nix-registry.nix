# adapted from https://github.com/NixOS/nixpkgs/blob/90a90b15a37d2cdc6f9eeb3e751febaf67d64145/nixos/modules/services/misc/nix-daemon.nix

{ config, lib, ... }:
with lib;
let cfg = config.nix;
in {
  options = {
    nix = {
      registry = mkOption {
        type = types.attrsOf (types.submodule (let
          inputAttrs = types.attrsOf
            (types.oneOf [ types.str types.int types.bool types.package ]);
        in { config, name, ... }: {
          options = {
            from = mkOption {
              type = inputAttrs;
              example = {
                type = "indirect";
                id = "nixpkgs";
              };
              description = "The flake reference to be rewritten.";
            };
            to = mkOption {
              type = inputAttrs;
              example = {
                type = "github";
                owner = "my-org";
                repo = "my-nixpkgs";
              };
              description =
                "The flake reference to which <option>from></option> is to be rewritten.";
            };
            flake = mkOption {
              type = types.unspecified;
              default = null;
              example = literalExample "nixpkgs";
              description = ''
                The flake input to which <option>from></option> is to be rewritten.
              '';
            };
            exact = mkOption {
              type = types.bool;
              default = true;
              description = ''
                Whether the <option>from</option> reference needs to match exactly. If set,
                a <option>from</option> reference like <literal>nixpkgs</literal> does not
                match with a reference like <literal>nixpkgs/nixos-20.03</literal>.
              '';
            };
          };
          config = {
            from = mkDefault {
              type = "indirect";
              id = name;
            };
            to = mkIf (config.flake != null) ({
              type = "path";
              path = config.flake.outPath;
            } // lib.filterAttrs (n: _v:
              n == "lastModified" || n == "rev" || n == "revCount" || n
              == "narHash") config.flake);
          };
        }));
        default = { };
        description = ''
          A user-wide flake registry.
        '';
      };
    };
  };
  config = {
    xdg.configFile."nix/registry.json".text = builtins.toJSON {
      version = 2;
      flakes =
        mapAttrsToList (_n: v: { inherit (v) from to exact; }) cfg.registry;
    };
  };
}

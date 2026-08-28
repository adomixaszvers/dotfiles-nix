{ pkgs, ... }:
{
  specs.treesitter.data = pkgs.vimPlugins.nvim-treesitter.withPlugins (
    p:
    builtins.attrValues {
      inherit (p)
        bash
        c
        lua
        nix
        vim
        ;
    }
  );
}

{ pkgs, ... }:
{
  specs.treesitter.data = pkgs.vimPlugins.nvim-treesitter.withPlugins (
    p: with p; [
      bash
      c
      lua
      nix
      vim
    ]
  );
}

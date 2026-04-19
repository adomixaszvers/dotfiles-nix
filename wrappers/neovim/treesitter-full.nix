{ pkgs, ... }:
{
  specs.treesitter.data = pkgs.vimPlugins.nvim-treesitter.withAllGrammars;
}

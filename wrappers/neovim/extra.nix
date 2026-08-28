{ pkgs, ... }:
{
  specs = {
    extra.data = [
      pkgs.vimPlugins.direnv-vim
    ];
    lsp = {
      data = [ pkgs.vimPlugins.nvim-lspconfig ];
      runtimePkgs = builtins.attrValues {
        inherit (pkgs)
          lua-language-server
          nil
          taplo
          ;
      };
    };
  };
}

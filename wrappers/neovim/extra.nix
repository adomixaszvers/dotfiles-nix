{ pkgs, ... }:
{
  specs = {
    extra.data = with pkgs.vimPlugins; [
      direnv-vim
    ];
    lsp = {
      data = [ pkgs.vimPlugins.nvim-lspconfig ];
      runtimePkgs = with pkgs; [
        lua-language-server
        nil
        taplo
      ];
    };
  };
}
